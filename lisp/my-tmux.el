;;
;; Copyright 2021  Mark Vogt
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
;; tmux, using the turnip package
;;
(require 'turnip)
(require 'my-region-or-line "kill-yank")
(require 'my-cur-word-or-region "grep-compile")

;; Same as turnip-yank-from-buffer, but update to work with latest tmux. The
;; format variable "line" doesn't work with the command tmux list-buffers, and
;; the command "tmux show-buffer -b foo" requires foo to be the buffer name
;; rather than its index.
(defun my-turnip-yank-from-buffer (&optional buffer)
  "Yank from tmux buffer with index BUFFER.
If no argument is provided the paste-buffer is used."
  (interactive
   (let* ((buffers (turnip:call->lines
                    "list-buffers" "-F" "#{buffer_name}: #{buffer_sample}"))
          (choice (completing-read "Buffer: " buffers nil 'confirm)))
     (list (when (string-match "^buffer[0-9]+" choice)
             (match-string-no-properties 0 choice)))
   )
  )
  (insert (apply #'turnip:call "show-buffer"
                 (when buffer (list "-b" buffer))))
)

(defvar my-turnip-send-hist '())
(defun my-turnip-send-text ()
  "Prompt for text, and send it to a tmux pane.
Pre-fill the prompt with the active region.
If no region is active, use the current line.
In both cases, trim leading and trailing whitespace."
  (interactive)
  (let ((cmd (read-string "Send text: " (my-region-or-line)
                          'my-turnip-send-hist))
        (target (call-interactively #'turnip-choose-pane)))
    (unless (called-interactively-p 'any)
      (setq target (turnip:normalize-and-check-target-pane target)))
    (turnip:send-keys target cmd)
  )
)

(defun my-turnip-send-cwd ()
  "After prompting, send cd command to a tmux pane.
Pre-fill the prompt with the directory of the current buffer."
  (interactive)
  (let ((cmd (read-string "Send text: " (format "cd %s" default-directory)
                          'my-turnip-send-hist))
        (target (call-interactively #'turnip-choose-pane)))
    (unless (called-interactively-p 'any)
      (setq target (turnip:normalize-and-check-target-pane target)))
    (turnip:send-keys target cmd)
  )
)

(defvar my-turnip-hostname-hist '())
(defun my-turnip-connect (cmd)
  "Open new tmux window with specified connection command. Prompt for hostname
to connect to. Use the hostname as the name of the new window.
Pre-fill the hostname prompt with the active region. If no region is active,
use the word currently at the point."
  (interactive)
  (let ((host (read-string (format "%s: " cmd) (my-cur-word-or-region)
                           'my-turnip-hostname-hist)))
    (turnip:call "new-window" "-n" host (format "%s %s" cmd host))
  )
)

;; https://github.com/abo-abo/hydra
(defhydra my-tmux-menu (:color blue)
  "
tmux:
_y_ Yank from tmux buffer         _S_ SSH in new tmux window
_t_ Send text to tmux pane        _R_ SSH with Ansible key in new tmux window
_d_ Send cd $PWD to tmux pane     _V_ SSH with vbldadm key in new tmux window
_r_ Send region to tmux pane      _T_ Telnet in new tmux window
_b_ Send region to tmux buffer    _p_ Select tmux pane for future commands
^ ^                               _c_ tmux command builder (C-RET to finalize)
"
  ("R" (my-turnip-connect "tmux-ssh-ansible") nil)
  ("S" (my-turnip-connect "tmux-ssh") nil)
  ("T" (my-turnip-connect "tmux-telnet") nil)
  ("V" (my-turnip-connect "tmux-ssh-vbldadm") nil)

  ;; FIXME-mvogt-20210502: The prompt for destination buffer is unintuitive.
  ;; It's asking for a name to create, and it gives a useless list of index
  ;; numbers for existing buffers. Replace this function with my own that
  ;; prompts in a different way.
  ("b" turnip-send-region-to-buffer nil)

  ("c" turnip-command nil)
  ("d" my-turnip-send-cwd nil)
  ("p" turnip-choose-pane nil)
  ("r" turnip-send-region nil)
  ("t" my-turnip-send-text nil)
  ("y" my-turnip-yank-from-buffer nil)
)

(global-set-key [?\M-g ?\M-t] 'my-tmux-menu/body)
