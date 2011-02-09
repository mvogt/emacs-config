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

;; Docs say to load browse-url before ffap.  I don't care about poor startup
;; time, and I use one ffap func that isn't declared with autoload.
(load "browse-url")
(load "ffap")
;; Activate special behavior with prefix instead of by default.
(setq ffap-require-prefix t)

;; ffap is much better than plain old find-file.
(global-set-key [?\C-x ?\C-f]     'find-file-at-point)
(global-set-key [f4]              'find-file-at-point)
(global-set-key (kbd "M-<f4>")    'ffap-other-window)
(global-set-key (kbd "<ESC><f4>") 'ffap-other-window)  ; for text console

(global-set-key [f2]              'save-buffer)
(global-set-key (kbd "M-<f2>")    'write-file)
(global-set-key (kbd "<ESC><f2>") 'write-file)         ; for text console
(global-set-key [?\C-x ?z]        (lambda () (interactive)
                                    (save-some-buffers t)))
(global-set-key [?\C-x ?\C-z]     (lambda () (interactive)
                                    (save-some-buffers t)
                                    (save-buffers-kill-emacs)))

;; The Emacs 23 daemon feature changes this to save-buffers-kill-terminal.  I
;; put it back to the original function here so I can kill the daemon with my
;; usual key sequence for exiting.
(global-set-key [?\C-x ?\C-c] 'save-buffers-kill-emacs)
