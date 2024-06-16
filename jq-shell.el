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
;; Run jq queries interactively against a source buffer or file.
;; The interface is similar to https://jqplay.org.
;;
;; The query buffer uses `jq-ts-mode'. Jq shell tries to guess if current query
;; syntax is error-free enough to run. Any errors or missing nodes from the
;; parse tree are highlighted similarly to flycheck.
;;
;; From the jq-shell, calling `jq-shell-menu' pops up a transient menu
;; for managing the session. There are commands to:
;;  - modify options passed to jq
;;  - define jq arguments (--arg/--argjson)
;;  - change output buffer format
;;  - change stdin
;;  - etc.
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

(defcustom jq-shell-default-output-mode
  (and (featurep 'json-ts-mode) 'json-ts-mode)
  "Default `major-mode' for output buffer."
  :type 'symbol)

(defcustom jq-shell-autorun nil
  "Eagerly run jq queries as they are entered."
  :type 'boolean)

(defcustom jq-shell-enable-key-completion t
  "Enable completion-at-point for keys."
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
  "C-c C-o" #'jq-shell-menu
  "C-c C-w" #'jq-shell-arrange-windows)

(define-minor-mode jq-shell-minor-mode
  "Mode active in jq shell."
  :keymap jq-shell-minor-mode-map
  (if (null jq-shell-minor-mode)
      (progn
        (remove-hook 'after-change-functions #'jq-shell-maybe-run t)
        (remove-hook 'completion-at-point-functions
                     #'jq-shell-completion-at-point t))
    (when jq-shell-autorun
      (add-hook 'after-change-functions #'jq-shell-maybe-run nil t))
    (when jq-shell-enable-key-completion
      (add-hook 'completion-at-point-functions
                #'jq-shell-completion-at-point nil t))))

(defvar jq-shell-sessions (make-hash-table :test #'equal)
  "All active jq shell sessions.")

;; Store jq shell session configuration
(cl-defstruct (jq-shell-session (:constructor jq-shell--make-session))
  shell                                       ; jq shell buffer
  region                                      ; (start . end) region to use from stdin
  stdin                                       ; stdin buffer
  stdout                                      ; stdout buffer
  stderr                                      ; stderr buffer
  (output-mode jq-shell-default-output-mode)  ; major mode for output buffer
  (args jq-shell-default-arguments)           ; jq arguments
  (stderr-file (make-temp-file "jq-shell")))  ; stderr file

(defvar-local jq-shell--session nil
  "Cached local jq shell session.")

(defvar-local jq-shell--stderr nil
  "Overlay to display jq stderr when errors are shown in shell.")

(defvar-local jq-shell--last-cmd nil
  "Stores last successfully run jq command.")

(defvar-local jq-shell--completion nil
  "Completion session.")

;; stored window configuration prior to starting jq shell
(defvar jq-shell--window-configuration nil)

(defun jq-shell--restore-window-configuration ()
  "Restore initial window configuration."
  (when jq-shell--window-configuration
    (set-window-configuration jq-shell--window-configuration))
  (setq jq-shell--window-configuration nil))

(defun jq-shell--arrange-windows (session)
  "Arrange windows for SESSION."
  (pcase-let (((cl-struct jq-shell-session shell stdin stdout) session))
    (when (bufferp stdin)
      (pop-to-buffer stdin nil t))
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
  (with-current-buffer (jq-shell-session-shell session)
    (jq-ts-mode)
    (jq-shell-minor-mode)
    (add-hook 'kill-buffer-hook #'jq-shell--cleanup nil t)
    (setq-local jq-shell--session session)))

(defun jq-shell--get-buffer (type stdin &optional hidden)
  "Setup output buffer of TYPE for STDIN.
If HIDDEN generate hidden buffers."
  (let* ((name (format "%sjq[%s]%s%s"
                       (if hidden " " "*")
                       (if (bufferp stdin) (buffer-name stdin) stdin)
                       (if (string-empty-p type) type (concat "::" type))
                       (if hidden "" "*")))
         (buf (get-buffer name)))
    (if (buffer-live-p buf) buf
      (with-current-buffer (get-buffer-create name)
        (erase-buffer)
        (current-buffer)))))

(defun jq-shell-make-session
    (&optional buffer-or-file region stdout stderr jq-shell no-setup hidden)
  "Create a new jq shell session for BUFFER-OR-FILE or current buffer.
Use REGION if non-nil, or current region if active, or whole buffer.
STDOUT, STDERR, and JQ-SHELL, when non-nil, specify buffers to use.
If NO-SETUP, return session without any setup.
HIDDEN is passed to `jq-shell--get-buffer'."
  (or buffer-or-file (setq buffer-or-file (current-buffer)))
  (when (bufferp buffer-or-file)
    (set-buffer buffer-or-file))
  (let ((session
         (jq-shell--make-session
          :stdin buffer-or-file
          :region (when (bufferp buffer-or-file)
                    (or region
                        (and (region-active-p) (car (region-bounds)))
                        (cons (point-min) (point-max))))
          :stdout (or stdout
                      (jq-shell--get-buffer "stdout" buffer-or-file hidden))
          :stderr (or stderr
                      (jq-shell--get-buffer "stderr" buffer-or-file hidden))
          :shell (or jq-shell (jq-shell--get-buffer "" buffer-or-file hidden)))))
    (if no-setup
        session
      (jq-shell--setup-shell session)
      (puthash (jq-shell-session-shell session) session jq-shell-sessions))))

(defun jq-shell--ensure-live (session)
  "Ensure buffers are available for SESSION."
  (pcase-let (((cl-struct jq-shell-session stdin stdout stderr) session))
    (unless (buffer-live-p stdout)
      (setf (jq-shell-session-stdout session)
            (jq-shell--get-buffer "stdout" stdin)))
    (unless (buffer-live-p stderr)
      (setf (jq-shell-session-stderr session)
            (jq-shell--get-buffer "stderr" stdin)))))

(defun jq-shell--cleanup (&optional buffer no-restore session)
  "Cleanup resources associated with jq shell BUFFER or SESSION.
Restore initial window configuration unless NO-RESTORE."
  (when-let* ((buffer (or buffer (current-buffer)))
              (session (or session (gethash buffer jq-shell-sessions))))
    (remhash buffer jq-shell-sessions)
    (pcase-let (((cl-struct jq-shell-session shell stdout stderr stderr-file)
                 session))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file))
      (with-current-buffer shell
        ;; cleanup completion session
        (let ((session jq-shell--completion))
          (setq jq-shell--completion nil)
          (and session (jq-shell--cleanup buffer t session))))
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
    (t :underline t :inherit error))
  "Jq shell face for errors.")

(defun jq-shell--make-error-overlay (beg end)
  "Make error overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'jq-shell-error)
    (overlay-put ov 'jq-shell t)))

(defun jq-shell--update-error-overlays ()
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
       (null (jq-shell--update-error-overlays))
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
  (let ((cmd (concat jq-shell-command " " args)))
    (concat (propertize cmd 'face 'font-lock-function-name-face) " '" jq-cmd "'")))

(defun jq-shell--arguments (args)
  "Format jq command line arguments ARGS."
  (mapconcat (lambda (arg)
               (if (stringp arg) arg
                 (let ((key (car arg)))
                   (mapconcat (lambda (a) (concat key " " a)) (cdr arg) " "))))
             args " "))

(defun jq-shell--update-stderr (status &optional session)
  "Update SESSION stderr according to STATUS."
  (pcase-let (((cl-struct jq-shell-session shell stderr stdout stderr-file)
               (or session jq-shell--session)))
   (with-current-buffer stderr
     (erase-buffer)
     (if (zerop status)
         (progn
           (when-let (win (get-buffer-window)) (delete-window win))
           (when jq-shell-errors-in-overlay
             (jq-shell--update-overlay shell 'jq-shell--stderr nil)))
       (save-excursion
         (insert-file-contents stderr-file)
         (goto-char (point-max))
         (insert (propertize "exit status" 'face 'error) ": "
                 (propertize (format "%s" status)
                             'font-lock-face 'font-lock-number-face)))
       (if jq-shell-errors-in-overlay
           (jq-shell--update-overlay shell 'jq-shell--stderr (buffer-string))
         (jq-shell--show-stderr stdout stderr))))))

(defun jq-shell--update-output (status &optional jq-cmd session)
  "Update SESSION buffers after command exited with STATUS.
Optionally, add JQ-CMD to jq command in stdout."
  (pcase-let (((cl-struct jq-shell-session stdout)
               (or session jq-shell--session)))
    (jq-shell--update-stderr status session)
    (when (zerop status)
      ;; Delete old input if query was successful and update command
      (with-current-buffer stdout
        (delete-region (point) (point-max))
        (goto-char (point-min))
        (setq header-line-format
              (jq-shell--format-command (car jq-cmd) (cdr jq-cmd))))
      (and jq-cmd
           (not (looking-at-p "null"))
           (setq jq-shell--last-cmd jq-cmd)))))

(defun jq-shell--prepare-output (session)
  "Setup output buffer for SESSION prior to run."
  (pcase-let (((cl-struct jq-shell-session stdout output-mode) session))
    (with-current-buffer stdout
      (when (and (fboundp output-mode)
                 (not (eq output-mode major-mode)))
        (funcall output-mode))
      (goto-char (point-min)))))

(defun jq-shell--call (cmd &optional session)
  "Call jq CMD in SESSION."
  (pcase-let (((cl-struct jq-shell-session region stdin stdout stderr-file)
               (or session jq-shell--session)))
    (if (bufferp stdin)
        (with-current-buffer stdin
          (call-process-region
           (car region) (cdr region)
           shell-file-name nil (list stdout stderr-file) nil
           shell-command-switch cmd))
      (process-file
       shell-file-name stdin (list stdout stderr-file) nil
       shell-command-switch cmd))))

(defun jq-shell--run (&optional beg end session)
  "Run jq cmd from BEG to END in jq shell buffer."
  (or session (setq session jq-shell--session))
  (jq-shell--ensure-live session)
  (jq-shell--prepare-output session)
  (pcase-let (((cl-struct jq-shell-session args) session))
    ;; FIXME: unnecessary if -S
    ;; (unless (buffer-live-p stdin)
    ;;   (user-error "Input buffer %s is dead" (buffer-name stdin)))
    (let* ((beg (or beg (point-min)))
           (end (or end (point-max)))
           (jq-cmd (string-trim (buffer-substring beg end)))
           (args (jq-shell--arguments args))
           (cmd (format "%s %s %s"
                        jq-shell-command args (shell-quote-argument jq-cmd)))
           (status (jq-shell--call cmd session)))
      (jq-shell--update-output status (cons args jq-cmd) session))))


;;; Completion

(defun jq-shell--setup-completion (&optional session)
  "Setup completion for SESSION."
  (pcase-let (((cl-struct jq-shell-session stdin region shell)
               (or session jq-shell--session)))
    (jq-shell-make-session stdin region nil nil shell t t)))

(defun jq-shell--get-key-completions (&optional session)
  "Return key completions for SESSION's last successful output."
  (when jq-shell--last-cmd
    (setq session (or session jq-shell--session))
    (or jq-shell--completion
        (setq jq-shell--completion (jq-shell--setup-completion session)))
    (pcase-let (((cl-struct jq-shell-session stdout) jq-shell--completion))
      (with-current-buffer stdout
        (erase-buffer))
      (let ((cmd (format "%s %s %s" jq-shell-command
                         (car jq-shell--last-cmd)
                         (shell-quote-argument
                          (concat (cdr jq-shell--last-cmd)
                                  " | keys[]")))))
        (when (zerop (jq-shell--call cmd jq-shell--completion))
          (with-current-buffer stdout
            (split-string (buffer-substring-no-properties
                           (point-min) (point-max)))))))))

(defun jq-shell-completion-at-point ()
  "Completion at point for keys."
  (let ((end (point))
        (beg (save-excursion
               (skip-syntax-backward "w_" (line-beginning-position))
               (and (eq ?. (char-before))
                    (point)))))
    (when beg
      (list beg end (jq-shell--get-key-completions)))))

;; -------------------------------------------------------------------
;;; Transient: Jq Menu for switches, options, arguments

(defun jq-shell--initial-arguments ()
  "Initial values for `jq-shell-menu'."
  (when jq-shell-minor-mode
    ;; FIXME: map :shortargs to :arguments
    (jq-shell-session-args jq-shell--session)))

(defclass jq-shell-variable (transient-lisp-variable)
  ((argument :initform "")
   (set-value :initarg :set-value
              :initform (lambda (sym val) (setf (slot-value jq-shell--session sym) val)))
   (format :initform " %k %d (%v)"))
  "Class used for jq options that update session.")

(defclass jq-shell-arguments (transient-option) ()
  "Class used for jq arguments.")

(cl-defmethod transient-init-value ((obj jq-shell-variable))
  (oset obj value (slot-value jq-shell--session (oref obj variable))))

(cl-defmethod transient-infix-read :around ((obj jq-shell-arguments))
  "Set `transient-read-with-initial-input' to t when reading args."
  (let ((transient-read-with-initial-input t))
    (cl-call-next-method obj)))

(transient-define-infix jq-shell--set-output-mode ()
  "Set output buffer's `major-mode'."
  :description "Set output mode"
  :class 'jq-shell-variable
  :variable 'output-mode
  :prompt "Mode: "
  :reader (lambda (prompt initial-input history)
            (intern (completing-read
                     prompt obarray
                     (lambda (sym) (string-match-p "-mode\\'" (symbol-name sym)))
                     t initial-input history))))

(transient-define-infix jq-shell--set-stdin ()
  "Set stdin buffer."
  :description "Set stdin"
  :class 'jq-shell-variable
  :variable 'stdin
  :prompt "Stdin: "
  :reader (lambda (prompt initial-input _history)
            (get-buffer (read-buffer prompt initial-input t))))

(transient-define-infix jq-shell--arg ()
  "Define jq --arg arguments, eg. '--arg k v'."
  :description "Set arg(s)"
  :class 'jq-shell-arguments
  :argument "--arg "
  :multi-value 'repeat
  :prompt "--arg k v[,k v]*: ")

(transient-define-infix jq-shell--argjson ()
  "Define jq --argjson arguments."
  :description "Set argjson(s)"
  :class 'jq-shell-arguments
  :argument "--argjson "
  :multi-value 'repeat
  ;; XXX: nicer way to read json values?
  :prompt "--argjson k v[,k v]*: ")

(transient-define-suffix jq-shell--set-arguments (args &optional run)
  "Set jq arguments for current session."
  (interactive (list (transient-args 'jq-shell-menu)
                     (equal "<return>" (oref (transient-suffix-object) key))))
  (setf (jq-shell-session-args jq-shell--session) args)
  (and run (jq-shell-run)))

(transient-define-prefix jq-shell-menu ()
  "Show menu for jq shell."
  :value #'jq-shell--initial-arguments
  [ :if-non-nil jq-shell-minor-mode
    "Switches"
    ("-r" "Output raw strings" ("-r" "--raw-output"))
    ("-R" "Read raw strings" ("-R" "--raw-input"))
    ("-s" "Slurp" ("-s" "--slurp"))
    ("-n" "Use 'null' as input value" ("-n" "--null-input"))
    ("-e" "Set exit status" ("-e" "--exit-status"))
    ("-L" "Add directory to search list" "-L"
     :class transient-option :multi-value 'repeat)]
  [ :if-non-nil jq-shell-minor-mode
    ["Formatting"
     ("-a" "Ascii" ("-a" "--ascii-output"))
     ("-j" "Join" ("-j" "--join-output"))
     ("-c" "Compact" ("-c" "--compact-output"))
     ("-C" "Color" ("-C" "--color-output"))
     ("-M" "Monochrome" ("-M" "--monochrome-output"))
     ("-S" "Sort keys" ("-S" "--sort-keys"))
     ("=t" "Indent with tabs" "--tab")
     ("=i" "Set indent level" "--indent " :class transient-option)]
    ["Buffers"
     (":s" jq-shell--set-stdin)
     (":o" jq-shell--set-output-mode)]]
  [ :if-non-nil jq-shell-minor-mode
    "Arguments"
    (":a" jq-shell--arg)
    (":j" jq-shell--argjson)
    ;; XXX: handle these? or just have an infix for values to append to command
    ;; ("--args")
    ;; ("--jsonargs" "")
    ]
  [[ :if-non-nil jq-shell-minor-mode "Commands"
     ("s" "Set" jq-shell--set-arguments)
     ("<return>" "Set and run" jq-shell--set-arguments)]]
  (interactive)
  (if jq-shell-minor-mode
      (transient-setup 'jq-shell-menu)
    (user-error "Call from jq-shell buffer")))

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
  (with-current-buffer (jq-shell-session-stdout jq-shell--session)
    (erase-buffer)))

(defun jq-shell-arrange-windows ()
  "Arrange windows for current session."
  (interactive nil jq-shell-minor-mode-map)
  (jq-shell--arrange-windows jq-shell--session))

;;;###autoload
(defun jq-shell (&optional buffer-or-file region)
  "Run jq interactively on current buffer or choose input file.
With \\[universal-argument] interactively choose input BUFFER-OR-FILE.

If REGION is non-nil, or there is an active region in the current buffer,
jq input is restricted to that region. REGION is a cons cell specifying the
start and end of the region."
  (interactive
   (let ((buf (if current-prefix-arg
                  (read-file-name "Input file: ")
                (current-buffer))))
     (list buf (unless current-prefix-arg
                 (if (region-active-p) (car (region-bounds))
                   (cons (point-min) (point-max)))))))
  (setq jq-shell--window-configuration (current-window-configuration))
  (condition-case-unless-debug err
      (jq-shell--arrange-windows (jq-shell-make-session buffer-or-file region))
    (error (jq-shell--restore-window-configuration)
           (error "%s" (error-message-string err)))
    (quit (jq-shell--restore-window-configuration))))

(provide 'jq-shell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; jq-shell.el ends here
