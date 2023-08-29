;;; jq-shell.el --- Run jq queries on buffer -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/jq-shell
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (jq-ts-mode "1.0") (json-mode "1.6"))
;; Created: 28 August 2023

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Run jq queries interactively against a source buffer.
;;
;; Uses tree sitter (`jq-ts-mode') to guess if current query syntax is
;; error-free enough to run.
;;
;; The jq shell uses `jq-ts-mode' and the output is `json-mode'.
;;
;;; Code:

(require 'jq-ts-mode)
(require 'json-mode)

(defgroup jq-shell nil
  "Run jq interactively on buffer."
  :group 'tools)

(defcustom jq-shell-command "jq"
  "Command to run jq."
  :type 'string
  :group 'jq-shell)

(defcustom jq-shell-default-options '()
  "Default options for running jq."
  :type '(repeat string)
  :group 'jq-shell)

(defvar jq-shell--window-configuration nil)

(defun jq-shell--restore ()
  "Restore initial window configuration."
  (when jq-shell--window-configuration
    (set-window-configuration jq-shell--window-configuration))
  (setq jq-shell--window-configuration nil))

(defun jq-shell--cleanup (buffers)
  "Create function to cleanup BUFFERS and restore window configuration."
  (cl-destructuring-bind (&key file stdout stderr &allow-other-keys) buffers
    (defalias 'jq-shell-cleanup
      `(lambda ()
         (when (file-exists-p ,file)
           (delete-file ,file))
         (dolist (buf '(,stdout ,stderr))
           (and (buffer-live-p buf) (kill-buffer buf)))
         (jq-shell--restore)))))

(defun jq-shell-should-run-p ()
  "Non-nil when no errors are detected in tree sitter parse tree."
  (null (treesit-search-subtree (treesit-buffer-root-node) "ERROR")))

(defun jq-shell-maybe-run (&rest _)
  "Run current query when no syntax errors detected."
  (when (jq-shell-should-run-p)
    (jq-shell--run)))

(defun jq-shell-run ()
  "Manually run current jq query."
  (interactive)
  (and (fboundp 'jq-shell--run) (jq-shell--run)))

(defun jq-shell-quit ()
  "Exit shell and cleanup, restoring initial window configuration."
  (interactive)
  (let (kill-buffer-query-functions)
    (kill-buffer (current-buffer))))

(defvar-keymap jq-shell-minor-mode-map
  :doc "Keymap in jq shell."
  "C-c C-k" #'jq-shell-quit
  "C-c C-c" #'jq-shell-run)

(define-minor-mode jq-shell-minor-mode
  "Mode active in jq shell."
  :keymap jq-shell-minor-mode-map
  (if jq-shell-minor-mode
      (add-hook 'after-change-functions #'jq-shell-maybe-run nil t)
    (remove-hook 'after-change-functions #'jq-shell-maybe-run t)))

(defun jq-shell--create-buffers (source-buf)
  "Create resources for SOURCE-BUF."
  (let ((name (format "jq[%s]" (buffer-name source-buf))))
    (plist-put  
     (cl-loop for (key . suffix) in '((:shell . "")
                                      (:stdout . "::stdout")
                                      (:stderr . "::stderr"))
              nconc (list key (get-buffer-create (concat "*" name suffix "*"))))
     :file (make-temp-file "jq-shell"))))

(defvar-local jq-shell--errors nil
  "Overlay to display jq stderr.")

(defun jq-shell--update (error-p buffers)
  "Update BUFFERS according to ERROR-P."
  (cl-destructuring-bind (&key shell stdout stderr file) buffers
    (with-current-buffer shell
      ;; FIXME: better error placement so it doesn't move query,
      ;; popup minibuffer or pinned to bottom/right
      ;; update error overlay
      (overlay-put jq-shell--errors 'after-string
                   (when error-p
                     (with-current-buffer stderr
                       (erase-buffer)
                       (insert-file-contents file)
                       (buffer-string)))))
    ;; delete old input if query was successful
    (unless error-p
      (with-current-buffer stdout
        (delete-region (point) (point-max))
        (goto-char (point-min))))))

(defun jq-shell--setup (start end source)
  "Setup jq shell.
Input comes from SOURCE in region START to END."
  (let ((bufs (jq-shell--create-buffers source)))
    (cl-destructuring-bind (&key shell stdout file &allow-other-keys) bufs
      (with-current-buffer shell
        (add-hook 'kill-buffer-hook (jq-shell--cleanup bufs) nil t)
        (jq-ts-mode)
        (jq-shell-minor-mode)
        (setq jq-shell--errors
              (make-overlay (point-max) (point-max) nil 'front-advance))
        (overlay-put jq-shell--errors 'invisible t)
        (fset 'jq-shell--run
              `(lambda (&optional qbeg qend)
                 (cl-assert (buffer-live-p ,source))
                 (with-current-buffer ,stdout
                   (goto-char (point-min)))
                 (let* ((qbeg (or qbeg (point-min)))
                        (qend (or qend (point-max)))
                        (cmd (buffer-substring qbeg qend))
                        (status
                         (with-current-buffer ,source
                           (call-process-region
                            ,start ,end
                            shell-file-name
                            nil
                            (list ,stdout ,file)
                            nil
                            shell-command-switch
                            (format
                             "%s %s %s %s"
                             jq-shell-command
                             (mapconcat 'identity jq-shell-default-options " ")
                             ;; FIXME: args
                             "-r"
                             (shell-quote-argument cmd))))))
                   (jq-shell--update (not (zerop status)) ',bufs))))
        bufs))))


;;;###autoload
(defun jq-shell (beg end buffer)
  "Run jq interactively on region from BEG to END in BUFFER."
  (interactive
   (let ((buf (if current-prefix-arg
                  (read-buffer "Source buffer: ")
                (current-buffer))))
     (if (region-active-p)
         (list (region-beginning) (region-end) buf)
       (list (point-min) (point-max) buf)))
   json-mode)
  (setq jq-shell--window-configuration (current-window-configuration))
  (condition-case-unless-debug err
      (cl-destructuring-bind
          (&key shell stdout &allow-other-keys) (jq-shell--setup beg end buffer)
        ;; (add-hook 'kill-buffer-hook (apply #'jq-shell--cleanup bufs) nil t)
        (with-current-buffer stdout
          (erase-buffer)
          (json-mode))
        (delete-other-windows)
        (window--display-buffer stdout (split-window-horizontally) 'window)
        (select-window (display-buffer-in-side-window shell nil)))
    (error
     (jq-shell--restore)
     (error "%s" (error-message-string err)))
    (quit (jq-shell--restore))))

(provide 'jq-shell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; jq-shell.el ends here
