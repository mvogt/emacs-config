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
;; Load, save, exit
;;
;; I often hit this key combination accidentally.
(setq confirm-kill-emacs 'y-or-n-p)

;; The ffap docs say to load browse-url before ffap. I don't care about poor
;; startup time.
(load "browse-url")
(load "ffap")
;; Activate special behavior with prefix instead of by default.
(setq ffap-require-prefix t)

(defun my-save-all-kill-emacs ()
  "Unconditionally save all buffers and exit Emacs."
  (interactive)
  (save-some-buffers t)
  (save-buffers-kill-emacs)
)

(recentf-mode 1)
;; I never want to save my recent files to disk. The only way I see to do that
;; is override the internal recentf.el save function to a nop.
(defun recentf-save-list ()
  (interactive)
)

;; Keeping this around for the times when I don't want Helm's file opener.
(global-set-key [?\C-x ?i]        'find-file-at-point)
(global-set-key [?\C-x ?\M-i]     'find-file-at-point)

(global-set-key [?\M-s]           'save-buffer)
(global-set-key [?\C-x ?\M-s]     'save-buffer)
(global-set-key [f2]              'save-buffer)
(global-set-key (kbd "M-<f2>")    'write-file)
(global-set-key (kbd "<ESC><f2>") 'write-file)         ; for text console
(global-set-key [?\C-x ?z]        (lambda () (interactive)
                                    (save-some-buffers t)))
(global-set-key [?\C-x ?\C-z]     'my-save-all-kill-emacs)
(global-set-key [?\C-x ?\M-z]     'my-save-all-kill-emacs)

;; The Emacs 23 daemon feature changes this to save-buffers-kill-terminal.  I
;; put it back to the original function here so I can kill the daemon with my
;; usual key sequence for exiting.
(global-set-key [?\C-x ?\C-c] 'save-buffers-kill-emacs)
(global-set-key [?\C-x ?\M-c] 'save-buffers-kill-emacs)
