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
  :prefix "jq-shell-"
  :group 'tools)

(defcustom jq-shell-command "jq"
  "Command to run jq."
  :type 'string)

(defcustom jq-shell-default-options '()
  "Default options for running jq."
  :type '(repeat string))

(defcustom jq-shell-autorun t
  "Eagerly run jq queries as they are entered."
  :type 'boolean)

(defcustom jq-shell-errors-in-overlay nil
  "When nil, jq errors are displayed in stderr buffer next to shell buffer.
When non-nil, display errors in overlay in shell buffer instead."
  :type 'boolean)

(defvar jq-shell-sessions (make-hash-table :test #'equal)
  "All active jq shell sessions.")

;; Store jq shell session configuration
(cl-defstruct (jq-shell--session (:constructor jq-shell--make-session))
  shell                                       ; jq shell buffer
  region                                      ; (start . end) region to use from stdin
  stdin                                       ; stdin buffer
  stdout                                      ; stdout buffer
  stderr                                      ; stderr buffer
  (stderr-file (make-temp-file "jq-shell")))  ; stderr file

(defvar-local jq-shell-session nil
  "Cached local jq shell session.")

(defvar-local jq-shell--errors nil
  "Overlay to display jq stderr when errors are shown in shell.")

;; stored window configuration prior to starting jq shell
(defvar jq-shell--window-configuration nil)

(defun jq-shell--restore-window-configuration ()
  "Restore initial window configuration."
  (when jq-shell--window-configuration
    (set-window-configuration jq-shell--window-configuration))
  (setq jq-shell--window-configuration nil))

(defun jq-shell--arrange-windows (session)
  "Arrange windows for SESSION."
  (pcase-let (((cl-struct jq-shell--session shell stdin stdout) session))
    (pop-to-buffer stdin nil t)
    (delete-other-windows)
    (window--display-buffer stdout (split-window-horizontally) 'window)
    (select-window
     (window--display-buffer shell (split-window-below -15) 'window))))

(defun jq-shell--show-stderr (stdout stderr)
  "Show STDERR buffer in lower part of STDOUT buffer."
  (unless (get-buffer-window stderr)
    (with-selected-window (get-buffer-window stdout)
      (window--display-buffer stderr (split-window-below -15) 'window))))

(defun jq-shell--update-overlay (buffer &optional str)
  "Update error overlay in BUFFER with STR."
  (with-current-buffer buffer
    (unless (overlayp jq-shell--errors)
      (setq jq-shell--errors
            (make-overlay (point-min) (point-min) (current-buffer) 'front))
      (overlay-put jq-shell--errors 'invisible t))
    (overlay-put jq-shell--errors 'after-string (and str (concat str "\n")))))

(defun jq-shell--setup-shell (session)
  "Setup jq shell buffer for SESSION."
  (with-current-buffer (jq-shell--session-shell session)
    (jq-ts-mode)
    (jq-shell-minor-mode)
    (add-hook 'kill-buffer-hook #'jq-shell--cleanup nil t)
    (setq-local jq-shell-session session)))

(defun jq-shell-make-session (&optional buffer region)
  "Create a new jq shell session for BUFFER or current buffer.
Use REGION if non-nil, or current region if active, or whole buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (name (buffer-name buffer))
         (bname (format "jq[%s]" name)))
    (set-buffer buffer)
    (cl-macrolet ((jq:buffer (&optional suffix &rest body)
                    `(with-current-buffer (get-buffer-create
                                           (concat "*" bname ,suffix "*"))
                       (prog1 (current-buffer) ,@body))))
      (let ((session
             (jq-shell--make-session
              :stdin buffer
              :region (or region
                          (and (region-active-p) (car (region-bounds)))
                          (cons (point-min) (point-max)))
              :stdout (jq:buffer "::stdout"
                                 (erase-buffer)
                                 (json-mode))
              :stderr (jq:buffer "::stderr" (erase-buffer))
              :shell (jq:buffer nil (erase-buffer)))))
        (jq-shell--setup-shell session)
        (puthash (jq-shell--session-shell session) session jq-shell-sessions)))))

(defun jq-shell--cleanup (&optional buffer no-restore)
  "Cleanup resources associated with jq shell BUFFER.
Restore initial window configuration unless NO-RESTORE."
  (when-let* ((buffer (or buffer (current-buffer)))
              (session (gethash buffer jq-shell-sessions)))
    (remhash buffer jq-shell-sessions)
    (pcase-let (((cl-struct jq-shell--session shell stdout stderr stderr-file)
                 session))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file))
      (dolist (buf (list shell stdout stderr))
        (and (buffer-live-p buf)
             (kill-buffer buf))))
    (unless no-restore (jq-shell--restore-window-configuration))))

(defun jq-shell-cleanup-all (&optional restore)
  "Cleanup all jq shell buffers and restore window configuration with RESTORE."
  (interactive "P")
  (maphash (lambda (k _v) (jq-shell--cleanup k t)) jq-shell-sessions)
  (and restore (jq-shell--restore-window-configuration)))

(defun jq-shell-should-run-p ()
  "Non-nil when no errors are detected in tree sitter parse tree."
  (and jq-shell-minor-mode
       (null (treesit-search-subtree (treesit-buffer-root-node) "ERROR"))
       (let ((node (treesit-node-at (point))))
         ;; tree-sitter doesn't currently have a way to match MISSING nodes
         ;; directly
         (if (treesit-node-match-p node "identifier")
             (not (string-empty-p (treesit-node-text node)))
           t))))

(defun jq-shell-maybe-run (&rest _)
  "Run current query when no syntax errors detected."
  (and (jq-shell-should-run-p) (jq-shell--run)))

(defun jq-shell--update-output (status)
  "Update session buffers after command exited with STATUS."
  (and (zerop status) (setq status nil))
  (pcase-let* (((cl-struct jq-shell--session shell stderr stdout stderr-file)
                jq-shell-session))
    (with-current-buffer stderr
      (erase-buffer)
      (if (null status)
          (when-let (win (get-buffer-window))
            (delete-window win))
        (insert-file-contents stderr-file)
        (goto-char (point-min))
        (if jq-shell-errors-in-overlay
            (jq-shell--update-overlay shell (buffer-string))
          (jq-shell--show-stderr stdout stderr))))
    (unless status
      ;; delete old input if query was successful
      (with-current-buffer stdout
        (delete-region (point) (point-max))
        (goto-char (point-min))))))

(defun jq-shell--run (&optional beg end)
  "Run jq cmd from BEG to END in jq shell buffer."
  (pcase-let (((cl-struct jq-shell--session region stdin stdout stderr-file)
               jq-shell-session))
    (unless (buffer-live-p stdin)
      (user-error "Input buffer %s is dead" stdin))
    (let* ((beg (or beg (point-min)))
           (end (or end (point-max)))
           (jq-cmd (buffer-substring beg end))
           (cmd (format "%s %s %s %s"
                        jq-shell-command
                        (mapconcat 'identity jq-shell-default-options " ")
                        ;; FIXME: args
                        "-r"
                        (shell-quote-argument jq-cmd))))
      (jq-shell--update-output
       (with-current-buffer stdin
         (call-process-region
          (car region) (cdr region) shell-file-name nil (list stdout stderr-file) nil
          shell-command-switch cmd))))))

;; -------------------------------------------------------------------
;;; Commands

(defun jq-shell-run (&optional prefix)
  "Manually run current jq query.
With \\[universal-argument] PREFIX, run query before point."
  (interactive nil jq-shell-minor-mode)
  (if prefix (jq-shell--run (point-min) (point))
    (jq-shell--run)))

(defun jq-shell-quit ()
  "Exit shell and cleanup, restoring initial window configuration."
  (interactive nil jq-shell-minor-mode)
  (when jq-shell-minor-mode
    (let (kill-buffer-query-functions)
      (kill-buffer (current-buffer)))))

(defun jq-shell-toggle-autorun ()
  "Toggle auto-running jq query on/off."
  (interactive nil jq-shell-minor-mode)
  (when jq-shell-minor-mode
    (if (memq #'jq-shell-maybe-run after-change-functions)
        (remove-hook 'after-change-functions #'jq-shell-maybe-run t)
      (add-hook 'after-change-functions #'jq-shell-maybe-run nil t))))

(defun jq-shell-clear-output ()
  "Clear output buffer associated with shell."
  (interactive nil jq-shell-minor-mode-map)
  (with-current-buffer (jq-shell--session-stdout jq-shell-session)
    (erase-buffer)))

(defvar-keymap jq-shell-minor-mode-map
  :doc "Keymap in jq shell."
  "C-c C-k" #'jq-shell-quit
  "C-c C-a" #'jq-shell-toggle-autorun
  "C-c C-l" #'jq-shell-clear-output
  "C-c C-c" #'jq-shell-run)

(define-minor-mode jq-shell-minor-mode
  "Mode active in jq shell."
  :keymap jq-shell-minor-mode-map
  (if jq-shell-minor-mode
      (when jq-shell-autorun
        (add-hook 'after-change-functions #'jq-shell-maybe-run nil t))
    (remove-hook 'after-change-functions #'jq-shell-maybe-run t)))

;;;###autoload
(defun jq-shell (&optional buffer region)
  "Run jq interactively on current buffer.
With \\[universal-argument] interactively choose input BUFFER.

If REGION is non-nil, or there is an active region in the current buffer,
jq input is restricted to that region. REGION is a cons cell specifying the
start and end of the region."
  (interactive
   (let ((buf (if current-prefix-arg
                  (get-buffer (read-buffer "Source buffer: " nil t))
                (current-buffer))))
     (list buf  (unless current-prefix-arg
                  (if (region-active-p) (car (region-bounds))
                    (cons (point-min) (point-max)))))))
  (setq jq-shell--window-configuration (current-window-configuration))
  (condition-case-unless-debug err
      (jq-shell--arrange-windows (jq-shell-make-session buffer region))
    (error (jq-shell--restore-window-configuration)
           (error "%s" (error-message-string err)))
    (quit (jq-shell--restore-window-configuration))))

(provide 'jq-shell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; jq-shell.el ends here
