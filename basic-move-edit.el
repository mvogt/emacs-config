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
;; Basic movement and editing
;;
;; Turn off obnoxious jump scroll.
(setq scroll-conservatively 10)

;; Configure mouse scroll wheel.  If it doesn't work, the global-set-key
;; command below it should.
(mouse-wheel-mode 1)
;;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))
;;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))

;; Styled after other editors I have used.
(global-set-key [C-prior] 'beginning-of-buffer)
(global-set-key [C-next]  'end-of-buffer)
(global-set-key [C-home]  (lambda () (interactive) (move-to-window-line 0)))
(global-set-key [?\M-h]   (lambda () (interactive) (move-to-window-line 0)))
(global-set-key [C-end]   (lambda () (interactive) (move-to-window-line -1)))
(global-set-key [?\M-l]   (lambda () (interactive) (move-to-window-line -1)))
(global-set-key [home]    'move-beginning-of-line)
(global-set-key [end]     'move-end-of-line)
(global-set-key [?\M-m]   'move-to-window-line)   ; middle of window
(global-set-key [?\M-g]   'goto-line)

;; Set S-up/down (arrow keys) to scroll the current window up/down one line.
;; I used to set C-up/down for the same thing, but I found it interferes with
;; some local modes.
(global-set-key [S-down] (lambda () (interactive) (scroll-up 1)))
(global-set-key [S-up]   (lambda () (interactive) (scroll-down 1)))
(global-set-key [?\M-N]  (lambda () (interactive) (scroll-up 1)))
(global-set-key [?\M-P]  (lambda () (interactive) (scroll-down 1)))

(global-set-key [?\M-n] 'next-line)
(global-set-key [?\M-p] 'previous-line)

(global-set-key [?\M-a] 'move-beginning-of-line)
(global-set-key [?\M-e] 'move-end-of-line)

(global-set-key [?\M-A] 'backward-paragraph)
(global-set-key [?\M-E] 'forward-paragraph)

(global-set-key [?\C-x ?\M-a] 'backward-sentence)
(global-set-key [?\C-x ?\M-e] 'forward-sentence)

(global-set-key [?\M-V] 'scroll-up)
(global-set-key [?\M-L] 'recenter-top-bottom)

(global-set-key [?\C-x ?\M-o] 'delete-blank-lines)

;; At one point, under Exceed or maybe Cygwin/Xming, the delete key was
;; defaulting to backspace instead of delete-char.
(global-set-key [delete] 'delete-char)
(global-set-key [?\M-D]  'delete-char)

(global-set-key [?\M-t]       'transpose-chars)
(global-set-key [?\M-T]       'transpose-words)
(global-set-key [?\C-x ?\M-t] 'transpose-lines)

(global-set-key [?\C-x ?\M-/] 'undo)

;; I find this to be almost useless, but it only works when mapped to a key.
(global-set-key [?\M-?] 'repeat)
