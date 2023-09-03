;;; jq-shell.el --- Run jq queries on buffer -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/jq-shell
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (jq-ts-mode "1.0")
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
;; Run jq queries interactively against a source buffer, similar to the
;; interface from https://jqplay.org.
;;
;; The Jq shell's major mode is `jq-ts-mode', which uses tree sitter. Jq shell
;; tries to guess if current query syntax is error-free enough to run. Any
;; errors or missing nodes from the parse tree are highlighted similarly to
;; flycheck.
;;
;; If `json-mode' is available, the result buffer will be `json-mode'.
;;
;;; Code:

(require 'transient)
(require 'jq-ts-mode)

(defgroup jq-shell nil
  "Run jq interactively on buffer."
  :prefix "jq-shell-"
  :group 'tools)

(defcustom jq-shell-command "jq"
  "Command to run jq."
  :type 'string)

(defcustom jq-shell-default-arguments '("--raw-output")
  ;; FIXME: how to make transient recognize short or long form args in
  ;; `:values'?
  "Default jq arguments (long-form) for running queries."
  :type '(repeat string))

(defcustom jq-shell-autorun t
  "Eagerly run jq queries as they are entered."
  :type 'boolean)

(defcustom jq-shell-errors-in-overlay nil
  "When nil, jq errors are displayed in stderr buffer next to shell buffer.
When non-nil, display errors in overlay in shell buffer instead."
  :type 'boolean)

(defvar-keymap jq-shell-minor-mode-map
  :doc "Keymap in jq shell."
  "C-c C-k" #'jq-shell-quit
  "C-c C-a" #'jq-shell-toggle-autorun
  "C-c C-l" #'jq-shell-clear-output
  "C-c C-c" #'jq-shell-run
  "C-c C-f" #'jq-shell-arguments
  "C-c C-w" #'jq-shell-arrange-windows)

(define-minor-mode jq-shell-minor-mode
  "Mode active in jq shell."
  :keymap jq-shell-minor-mode-map
  (if jq-shell-minor-mode
      (when jq-shell-autorun
        (add-hook 'after-change-functions #'jq-shell-maybe-run nil t))
    (remove-hook 'after-change-functions #'jq-shell-maybe-run t)))

(defvar jq-shell-sessions (make-hash-table :test #'equal)
  "All active jq shell sessions.")

;; Store jq shell session configuration
(cl-defstruct (jq-shell--session (:constructor jq-shell--make-session))
  shell                                       ; jq shell buffer
  region                                      ; (start . end) region to use from stdin
  stdin                                       ; stdin buffer
  stdout                                      ; stdout buffer
  stderr                                      ; stderr buffer
  (args jq-shell-default-arguments)           ; jq arguments
  (stderr-file (make-temp-file "jq-shell")))  ; stderr file

(defvar-local jq-shell-session nil
  "Cached local jq shell session.")

(defvar-local jq-shell--stderr nil
  "Overlay to display jq stderr when errors are shown in shell.")

(defvar-local jq-shell--command nil
  "Overlay to display the jq command that produced output.")

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
  "Show STDERR buffer in lower part of STDOUT buffer when visible or popup buffer."
  (unless (get-buffer-window stderr)
    (if-let (win (get-buffer-window stdout))
        (with-selected-window win
          (window--display-buffer stderr (split-window-below -15) 'window))
      (display-buffer-pop-up-window stderr nil))))

(defun jq-shell--update-overlay (buffer overlay &optional str placeholder _face)
  "Update header OVERLAY in BUFFER with STR or PLACEHOLDER."
  (with-current-buffer buffer
    (unless (and (boundp overlay) (overlayp (symbol-value overlay)))
      (overlay-put
       (set overlay (make-overlay (point-min) (point-min) (current-buffer) 'front))
       'invisible t))
    (let ((ov (symbol-value overlay))
          (border (make-string (window-body-width) ?―)))
      (overlay-put
       ov 'after-string (if str (concat str "\n" border)  placeholder)))))

(defun jq-shell--setup-shell (session)
  "Setup jq shell buffer for SESSION."
  (with-current-buffer (jq-shell--session-shell session)
    (jq-ts-mode)
    (jq-shell-minor-mode)
    (add-hook 'kill-buffer-hook #'jq-shell--cleanup nil t)
    (setq-local jq-shell-session session)))

(defun jq-shell--stdout-buffer (stdin)
  "Setup stdout for STDIN."
  (let* ((name (format "*jq[%s]::stdout*" (buffer-name stdin)))
         (buf (get-buffer name)))
    (if (buffer-live-p buf) buf
      (with-current-buffer (get-buffer-create name)
        (erase-buffer)
        (when (fboundp 'json-mode)
          (json-mode))
        (current-buffer)))))

(defun jq-shell--stderr-buffer (stdin)
  "Get or create stderr buffer for STDIN."
  (let* ((name (format "*jq[%s]::stderr*" (buffer-name stdin)))
         (buf (get-buffer name)))
    (if (buffer-live-p buf) buf
      (with-current-buffer (get-buffer-create name)
        (erase-buffer)
        (current-buffer)))))

(defun jq-shell-make-session (&optional buffer region)
  "Create a new jq shell session for BUFFER or current buffer.
Use REGION if non-nil, or current region if active, or whole buffer."
  (let ((buffer (or buffer (current-buffer))))
    (set-buffer buffer)
    (let ((session
           (jq-shell--make-session
            :stdin buffer
            :region (or region
                        (and (region-active-p) (car (region-bounds)))
                        (cons (point-min) (point-max)))
            :stdout (jq-shell--stdout-buffer buffer)
            :stderr (jq-shell--stderr-buffer buffer)
            :shell (get-buffer-create (format "*jq[%s]*" (buffer-name buffer))))))
      (jq-shell--setup-shell session)
      (puthash (jq-shell--session-shell session) session jq-shell-sessions))))

(defun jq-shell--ensure-live (session)
  "Ensure buffers are available for SESSION."
  (pcase-let (((cl-struct jq-shell--session stdin stdout stderr) session))
    (unless (buffer-live-p stdout)
      (setf (jq-shell--session-stdout session) (jq-shell--stdout-buffer stdin)))
    (unless (buffer-live-p stderr)
      (setf (jq-shell--session-stderr session) (jq-shell--stderr-buffer stdin)))))

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

(defvar jq-shell--errors-query
  (when (treesit-available-p)
    (treesit-query-compile 'jq '(((identifier) @id (:equal "" @id))
                                 (ERROR) @err)))
  "Tree sitter query to match errors and missing identifiers.")

(defface jq-shell-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Jq shell face for errors.")

(defun jq-shell--make-error-overlay (beg end)
  "Make error overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'jq-shell-error)
    (overlay-put ov 'jq-shell t)))

(defun jq-shell--update-errors ()
  "Update error overlays in jq shell buffer."
  (if-let ((ranges
            (treesit-query-range
             'jq jq-shell--errors-query (point-min) (point-max))))
      (prog1 t
        (dolist (range ranges)
          (jq-shell--make-error-overlay (car range) (1+ (cdr range)))))
    (remove-overlays (point-min) (point-max) 'jq-shell t)
    nil))

(defun jq-shell-should-run-p ()
  "Non-nil when no current command should run.
Currently, this function checks for errors or missing identifiers in tree
sitter parse tree."
  (and jq-shell-minor-mode
       (null (jq-shell--update-errors))
       ;; Note: error query won't pick up missing trailing node until a space is
       ;; entered
       (save-excursion
         (goto-char (point-max))
         (skip-syntax-backward " ")
         (not (memq (char-before) '(?| ?, ?\( ?\[ ?\{))))))

(defun jq-shell-maybe-run (&rest _)
  "Run current query when no syntax errors detected."
  (and (jq-shell-should-run-p) (jq-shell--run)))

(defun jq-shell--format-command (args jq-cmd)
  "Format jq cmd from ARGS and JQ-CMD."
  (let ((cmd (concat "jq " (jq-shell--arguments args))))
    (concat (propertize cmd 'face 'font-lock-function-name-face) " '" jq-cmd "'")))

(defun jq-shell--arguments (args)
  "Format jq command line arguments ARGS."
  (mapconcat (lambda (arg)
               (if (stringp arg) arg
                 (let ((key (car arg)))
                   (mapconcat (lambda (a) (concat key " " a)) (cdr arg) " "))))
             args " "))

(defun jq-shell--update-output (status &optional jq-cmd)
  "Update session buffers after command exited with STATUS.
Optionally, add JQ-CMD to jq command in stdout."
  (pcase-let* (((cl-struct jq-shell--session shell args stderr stdout stderr-file)
                jq-shell-session))
    (with-current-buffer stderr
      (erase-buffer)
      (if (zerop status)
          (progn
            (when-let (win (get-buffer-window)) (delete-window win))
            (and jq-shell-errors-in-overlay
                 (jq-shell--update-overlay shell 'jq-shell--stderr nil)))
        (save-excursion
          (insert-file-contents stderr-file)
          (goto-char (point-max))
          (insert (format "exit status: %s" status)))
        (if jq-shell-errors-in-overlay
            (jq-shell--update-overlay shell 'jq-shell--stderr (buffer-string))
          (jq-shell--show-stderr stdout stderr))))
    (when (zerop status)
      ;; delete old input if query was successful and update command
      (with-current-buffer stdout
        (delete-region (point) (point-max))
        (goto-char (point-min))
        (jq-shell--update-overlay
         stdout 'jq-shell--command (jq-shell--format-command args jq-cmd)
         "\n\n")))))

(defun jq-shell--run (&optional beg end)
  "Run jq cmd from BEG to END in jq shell buffer."
  (jq-shell--ensure-live jq-shell-session)
  (pcase-let (((cl-struct jq-shell--session region stdin stdout stderr-file args)
               jq-shell-session))
    (unless (buffer-live-p stdin)
      (user-error "Input buffer %s is dead" (buffer-name stdin)))
    (let* ((beg (or beg (point-min)))
           (end (or end (point-max)))
           (jq-cmd (buffer-substring beg end))
           (cmd (format "%s %s %s"
                        jq-shell-command
                        (jq-shell--arguments args)
                        ;; (mapconcat 'identity args " ")
                        (shell-quote-argument jq-cmd))))
      (jq-shell--update-output
       (with-current-buffer stdin
         (call-process-region
          (car region) (cdr region) shell-file-name nil (list stdout stderr-file) nil
          shell-command-switch cmd))
       jq-cmd))))

;; -------------------------------------------------------------------
;;; Jq Options / Arguments

(defun jq-shell-initial-arguments ()
  "Initial values for `jq-shell-arguments'."
  (when jq-shell-minor-mode
    (jq-shell--session-args jq-shell-session)))

(transient-define-suffix jq-shell-set-arguments (args &optional run)
  "Save jq arguments for current session."
  (interactive (list (transient-args 'jq-shell-arguments)
                     (equal "<return>" (oref (transient-suffix-object) key))))
  (setf (jq-shell--session-args jq-shell-session) args)
  (and run (jq-shell-run)))
  
;; (defclass jq-shell--arg (transient-option)
;;   ((reader :initform #'jq-shell--arg-reader)
;;    (always-read :initform t)
;;    (set-value :initarg :set-value :initform #'jq-shell--set-arg)))

;; (defun jq-shell--arg-reader (_prompt _initial-input _history)
;;   (format "%s %s"
;;           (read-string "Variable: ")
;;           (read-string "Value: ")))

;; (cl-defmethod transient-init-value ((obj jq-shell--arg))
;;   (or (cl-call-next-method obj) nil)
;;   (oset obj value (symbol-value (oref obj variable)))
;;   )

;; (cl-defmethod transient-infix-set ((obj jq-shell--arg) value)
;;   (funcall (oref obj set-value)
;;            ;; (oref obj variable)
;;            (oset obj value value)))

;; (cl-defmethod transient-format-description ((obj jq-shell--arg))
;;   (cl-call-next-method obj))

;; (cl-defmethod transient-format-value ((obj jq-shell--arg))
;;   (concat " --arg " (oref obj value)))

;; (defun jq-shell--set-arg (key val)
;;   (message "Setting %s = %s" key val))

;; (transient-define-infix jq-shell-arg ()
;;   :class 'jq-shell--arg
;; :variable "$var";; :initform
;; :value :initarg "value"
;; '(lambda () (read-string "Variable: "))
;; :multi-value t)

;; (transient-define-infix jq-shell-argjson ()
;;   :mutli-value t)

(transient-define-prefix jq-shell-arguments ()
  "Show menu for jq shell flags."
  :value #'jq-shell-initial-arguments
  ["Options"
   ("-r" "Output raw strings" ("-r" "--raw-output") :shortarg "-r")
   ("-R" "Read raw strings" ("-R" "--raw-input"))
   ("-c" "Compact Output" ("-c" "--compact-output"))
   ("-s" "Slurp" ("-s" "--slurp"))
   ("-n" "Use 'null' as input value" ("-n" "--null-input"))
   ("-S" "Sort keys" ("-S" "--sort-keys"))]
  ["Arguments (comma-separated for multiple, eg. a 1, b 2)"
   ;; FIXME: current args as initial input, additional infix command
   ;; to append args?
   ("a" "Set variable" "--arg" :class transient-option :multi-value t)
   ;; ("--argjson" "Set variable with JSON value" jq-shell-argjson)
   ;; ("--args")
   ;; ("--jsonargs" "")
   ]
  ["Commands"
   ("s" "Set" jq-shell-set-arguments)
   ("<return>" "Set and run" jq-shell-set-arguments)])

;; -------------------------------------------------------------------
;;; Commands

(defun jq-shell-run (&optional prefix)
  "Manually run current jq query.
With \\[universal-argument] PREFIX, run query before point."
  (interactive "P" jq-shell-minor-mode)
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

(defun jq-shell-arrange-windows ()
  "Arrange windows for current session."
  (interactive nil jq-shell-minor-mode-map)
  (jq-shell--arrange-windows jq-shell-session))

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
