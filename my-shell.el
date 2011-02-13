;;
;; Copyright 2011  Mark Vogt
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;----------------------------------------------------------------------------
;; Shell stuff
;;
;; Under Windows, use Cygwin's bash for our shell mode.
(when (and (boundp 'my-unix-p)
           (not my-unix-p)
           (file-directory-p "c:/cygwin"))
  (setq exec-path (cons "c:/cygwin/bin" exec-path))
  (setenv "PATH" (concat "c:\\cygwin\\bin;" (getenv "PATH")))
  (setq process-coding-system-alist '(("bash" . undecided-unix)))
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setq explicit-shell-file-name shell-file-name)
)

(setq my-shell-outbuf "*Shell Command Output*")

;; Unprintable chars to use as indicators when found in a string of user text.
(setq my-foreground-indicator (char-to-string #x07))
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
      (lambda () (interactive) (my-mb-prepend-exit my-foreground-indicator)))
    (define-key map [?\C-j]
      (lambda () (interactive) (my-mb-prepend-exit my-foreground-indicator)))
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
(provide 'my-read-shell-command)

(defun my-shell-command (command)
  "Look for special trailing chars in the minibuffer text (if any), interpret
them, and apply a hack similar to the ampersand suffix feature in the
shell-command function.  Adjust my-incomplete-cmdline as necessary."
  (cond
   ((string-equal (substring command 0 1) my-incomplete-indicator)
    ;; The user must have exited the minibuffer with Meta-Enter instead of
    ;; Enter.  Save the command for later, and don't execute it.
    (setq my-incomplete-cmdline (substring command 1))
    (setq shell-command-history (cdr shell-command-history))
   )
   ((string-equal (substring command 0 1) my-foreground-indicator)
    ;; The user must have exited the minibuffer with Ctrl-Enter instead of
    ;; Enter.  Just clean up the string (including in the history) and pass it
    ;; to shell-command.  Assuming the user didn't append an ampersand, this
    ;; will run in the foreground (i.e. synchronously).
    (setq my-incomplete-cmdline nil)
    (setq command (substring command 1))
    (setq shell-command-history (cons command (cdr shell-command-history)))
    ;; Same buffer for both sync and async command outputs.
    (shell-command command my-shell-outbuf)
   )
   (t
    ;; Normal command, but we're going to run it in the background by
    ;; appending an ampersand (if the user didn't already).
    (setq my-incomplete-cmdline nil)
    (shell-command (if (string-equal (substring command -1) "&")
                       command
                     (concat command "&"))
                   ;; Same buffer for both sync and async command outputs.
                   my-shell-outbuf)
   )
  )
)
(provide 'my-shell-command)

;; Redefine standard func with extra stuff at the beginning to work around an
;; annoying bug where async command output intermittently disappears because
;; the last line of the buffer is shown at the first line of the window.  This
;; switches to that window (if visible), displays the last line of the buffer
;; at the bottom of the window, and switches back to the original window.
(defun shell-command-sentinel (process signal)
  (if (memq (process-status process) '(exit signal))
      (let ((cur-win (get-buffer-window))
            (out-win (get-buffer-window my-shell-outbuf)))
        (when out-win
          (select-window out-win)
          (recenter -1)
          (select-window cur-win)
        )
        (message "%s: %s."
                 (car (cdr (cdr (process-command process))))
                 (substring signal 0 -1))
      )
  )
)

(require 'my-cur-word-or-region "grep-compile")
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
                   (get-buffer my-shell-outbuf)
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

(defun my-subshell ()
  "Launch a shell subprocess in a uniquely named buffer.
Prompt for the directory, and default to the current buffer's directory."
  (interactive)
  (save-match-data
    (let ((dir (read-file-name "Run interactive shell in: "
                               nil default-directory))
          (buf-name (concat "shell-" (buffer-name)))
          buf)
      (and (string-match "<[0-9]+>\\'" buf-name)
           (setq buf-name (substring buf-name 0 (match-beginning 0))))
      (setq buf (get-buffer-create (generate-new-buffer-name buf-name)))
      (switch-to-buffer-other-window buf)
      (shell buf)
      (force-mode-line-update)
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
(require 'set-tab-width "my-indent")
(add-hook 'sh-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ?\C-i] 'set-tab-width))
  )
)

(global-set-key [?\M-o]       'my-interactive-shell-command)
(global-set-key [?\M-O]       'select-shell-command-output-window)
(global-set-key [?\C-x ?\M-o] 'my-subshell)
