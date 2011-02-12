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

;; Set S-up/down (arrow keys) to scroll the current window up/down one line.
;; I used to set C-up/down for the same thing, but I found it interferes with
;; some local modes.
(global-set-key [S-down] (lambda () (interactive) (scroll-up 1)))
(global-set-key [S-up]   (lambda () (interactive) (scroll-down 1)))
(global-set-key [?\M-N]  (lambda () (interactive) (scroll-up 1)))
(global-set-key [?\M-P]  (lambda () (interactive) (scroll-down 1)))

;; Put these unused keys to good use.
(global-set-key [?\M-n] 'forward-paragraph)
(global-set-key [?\M-p] 'backward-paragraph)

;; Inverted "T" arrow keys on M-S-jkli.
(global-set-key [?\M-J] 'backward-char)
(global-set-key [?\M-L] 'forward-char)
(global-set-key [?\M-I] 'previous-line)
(global-set-key [?\M-K] 'next-line)

;; This is more useful than the default of duplicating backward/forward
;; paragraph like C-left/C-right does.
(global-set-key [M-left]  'backward-sentence)
(global-set-key [M-right] 'forward-sentence)

;; At one point, under Exceed or maybe Cygwin/Xming, the delete key was
;; defaulting to backspace instead of delete-char.
(global-set-key [delete] 'delete-char)

;; I find this to be almost useless, but it only works when mapped to a key.
(global-set-key [?\M-?] 'repeat)
