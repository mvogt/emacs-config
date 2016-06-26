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

;; Default command history is too short (30).
(setq history-length 100)

(setq my-shell-outbuf "*Shell Command Output*")

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
      (cd dir)
      (shell buf)
      (force-mode-line-update)
    )
  )
)

;; The original comint-{interrupt,quit,kill}-subjob functions don't work in a
;; buffer containing the output of shell-command. They first try and fail to
;; kill some input. This provides an alternate way to just send the desired
;; signal.
(defun my-kill-subshell (&optional sigtype)
  "Kill the current buffer's subprocess.

Sends one of three different signals depending on the optional prefix arg:
- No prefix: interrupt
- One universal prefixes: kill
- Two universal prefix: quit"
  (interactive "p")
  (cond
   ((= sigtype 1) (interrupt-process nil comint-ptyp))
   ((= sigtype 4) (kill-process nil comint-ptyp))
   ((= sigtype 16) (quit-process nil comint-ptyp))
  )
)

;; Interactive shell mode (command prompt window)
(add-hook 'shell-mode-hook
  (function (lambda ()
              ;; Additional keys to scroll the command history so I can use it
              ;; from a terminal that doesn't have the full keyboard support
              ;; that I'm accustomed to.
              (local-set-key [?\C-c ?\C-p] 'comint-previous-input)
              (local-set-key [?\C-c ?\C-n] 'comint-next-input)
              ;; Work-around for broken comint-{interrupt,quit,kill}-subjob.
              (local-set-key [?\C-c ?\C-k] 'my-kill-subshell))
  )
)

;; Make it easier to change the nominal indentation for a given buffer.
(defun set-sh-basic-offset (num-spaces)
  "Set the variable sh-basic-offset interactively."
  (interactive "NSet sh-basic-offset to: ")
  (or (null num-spaces)
      (set-variable (make-local-variable 'sh-basic-offset) num-spaces))
)

;; Shell (editing) mode
;; I assign C-c C-i globally to this function, but this mode happens to have a
;; pre-defined local assignment that overrides it.  So, we have to do this...
(require 'set-tab-width "my-indent")
(add-hook 'sh-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ?i] 'set-sh-basic-offset)
              (local-set-key [?\C-c ?\C-i] 'set-tab-width))
  )
)

(global-set-key [?\M-o]       'shell-command)
(global-set-key [?\C-x ?O]    'select-shell-command-output-window)
(global-set-key [?\C-x ?\M-o] 'my-subshell)
