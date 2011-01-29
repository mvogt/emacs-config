;;----------------------------------------------------------------------------
;; Shell stuff
;;
;; Under Windows, use Cygwin's bash for our shell mode.
(and (boundp 'my-unix-p)
     (not my-unix-p)
     (file-directory-p "c:/cygwin")
     (progn
       (setq exec-path (cons "c:/cygwin/bin" exec-path))
       (setenv "PATH" (concat "c:\\cygwin\\bin;" (getenv "PATH")))
       (setq process-coding-system-alist '(("bash" . undecided-unix)))
       (setq shell-file-name "bash")
       (setenv "SHELL" shell-file-name)
       (setq explicit-shell-file-name shell-file-name)
     )
)

;; Unprintable chars to use as indicators when found in a string of user text.
(setq my-background-indicator (char-to-string #x07))
(setq my-incomplete-indicator (char-to-string #x0c))

;; This variable must always be bound to something.
(setq my-incomplete-cmdline nil)

(defun my-mb-prepend-exit (str)
  "Helper func for my-minibuffer-local-shell-command-map."
  (move-beginning-of-line nil)
  (insert str)
  (exit-minibuffer)
)

;; This keymap is the same as minibuffer-local-shell-command-map, but it
;; defines some additional keys.
(defvar my-minibuffer-local-shell-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (if (>= emacs-major-version 23)
                               minibuffer-local-shell-command-map
                             minibuffer-local-map))
    (define-key map [C-return]
      (lambda () (interactive) (my-mb-prepend-exit my-background-indicator)))
    (define-key map [?\C-j]
      (lambda () (interactive) (my-mb-prepend-exit my-background-indicator)))
    (define-key map [M-return]
      (lambda () (interactive) (my-mb-prepend-exit my-incomplete-indicator)))
    (define-key map (kbd "C-M-j")
      (lambda () (interactive) (my-mb-prepend-exit my-incomplete-indicator)))
    map
  )
  "My keymap used for editing shell commands in the minibuffer."
)

(defun my-read-shell-command (prompt &optional initial-contents hist
                              &rest args)
  "Same as read-shell-command but uses my-minibuffer-local-shell-command-map
for the keymap in the minibuffer."
  (minibuffer-with-setup-hook
      (lambda ()
	(set (make-local-variable 'minibuffer-default-add-function)
	     'minibuffer-default-add-shell-commands)
      )
    (apply 'read-from-minibuffer prompt initial-contents
	   my-minibuffer-local-shell-command-map
	   nil
	   (or hist 'shell-command-history)
	   args)
  )
)

(defun my-shell-command (command)
  "Look for special trailing chars in the minibuffer text (if any), interpret
them, and apply a hack similar to the ampersand suffix feature in the
shell-command function.  Adjust my-incomplete-cmdline as necessary."
  (cond
   ((string-equal (substring command 0 1) my-incomplete-indicator)
    ;; The user must have exited the minibuffer with Meta-Enter instead of
    ;; Enter.  Save the command for later, and don't execute it.
    (setq command (substring command 1))
    (setq shell-command-history (cdr shell-command-history))
    (setq my-incomplete-cmdline command)
    )
   ((string-equal (substring command 0 1) my-background-indicator)
    ;; The user must have exited the minibuffer with Ctrl-Enter instead of
    ;; Enter.  Execute the command in a background child process with no
    ;; associated buffer for the output.  This doesn't benefit from several
    ;; advanced features in shell-command, such as remote buffers.
    (setq command (substring command 1))
    (setq shell-command-history (cons command (cdr shell-command-history)))
    (setq my-incomplete-cmdline nil)
    (start-process "Shell" nil shell-file-name shell-command-switch command)
    )
   (t
    (setq my-incomplete-cmdline nil)
    (shell-command command)
    )
   )
)

(defun my-interactive-shell-command (append-p)
  "Wrapper for shell-command that saves the command without running it if
the minibuffer is exited with Meta-Enter instead of Enter.
The initial command text to be edited is the previous command if it wasn't
run (because of that key sequence).

With a non-nil prefix, append to that initial command using text from the
current buffer.  If the current region is active, append it.
Otherwise, append the word surrounding the point.

Variable my-incomplete-cmdline stores the command that has not yet run.

If the minibuffer is exited with Ctrl-Enter instead of Enter, run the command
in the background without a buffer showing its output."
  (interactive "P")
  (my-shell-command (my-read-shell-command
                     "Shell command: "
                     (concat (or my-incomplete-cmdline "")
                             (if (null append-p) ""
                               (if (null my-incomplete-cmdline)
                                   (my-cur-word-or-region)
                                 (concat " " (my-cur-word-or-region)))))))
)

(defun select-shell-command-output-window (&optional buffer)
  "Select the window showing BUFFER, and jump to the end.
If buffer is omitted, select the shell command output buffer."
  (interactive)
  (setq buffer (if (null buffer)
                   (get-buffer "*Shell Command Output*")
                 (if (bufferp buffer) buffer (get-buffer buffer))
               )
  )
  (if (null buffer)
      (message "Buffer not found")
    (display-buffer buffer)
    (let ((window-list (get-buffer-window-list buffer 'nomini nil)))
      (and window-list
           (select-window (car window-list))
           (goto-char (point-max))
      )
    )
  )
)

;; Interactive shell mode (command prompt window)
;; Set additional keys to scroll the command history so I can use it from a
;; terminal that doesn't have the full keyboard support that I'm accustomed
;; to.
(add-hook 'shell-mode-hook
  (function (lambda ()
             (local-set-key [?\C-c ?\C-p] 'comint-previous-input)
             (local-set-key [?\C-c ?\C-n] 'comint-next-input))
  )
)

;; Shell mode
;; I assign C-c C-i globally to this function, but this mode happens to have a
;; pre-defined local assignment that overrides it.  So, we have to do this...
(add-hook 'sh-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ?\C-i] 'set-tab-width))
  )
)

(global-set-key [?\M-!] 'my-interactive-shell-command)
(global-set-key [?\M-`] 'my-interactive-shell-command)
(global-set-key [?\M-~] 'select-shell-command-output-window)
