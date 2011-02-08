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
;; Appearance (not including the overall frame)
;;
;; Don't ever make audible sounds.
(setq visible-bell 't)

;; Highlight matching parenthesis.
(show-paren-mode 1)

;; Display the column number in the mode line.
(column-number-mode 1)

;; Right margin for fill-paragraph.
(setq-default fill-column 78)

;; When a frame is at least this wide, commands that split it into two windows
;; will create a side-by-side split.  Otherwise, they'll create a top-bottom
;; split.
(setq split-width-threshold 180)

;; Controls the vertical split between two windows.
(global-set-key [?\M-{] 'shrink-window-horizontally)
(global-set-key [?\M-}] 'enlarge-window-horizontally)

;; Toggle various kinds of line wrap.
(if (>= emacs-major-version 23)
    (global-set-key [?\C-x ?W] (lambda () (interactive)
                                 (setq line-move-visual
                                       (if line-move-visual nil t))))
  (if (file-accessible-directory-p my-3rd-party-elisp-path)
      (progn
        (autoload 'visible-lines-mode "visible-lines")
        (global-set-key [?\C-x ?W] 'visible-lines-mode)
      )
  )
)
(global-set-key [?\C-x ?w]    'toggle-truncate-lines)
(global-set-key [?\C-x ?\M-w]
                (lambda () (interactive) ; for vert split (C-x 3)
                  (setq truncate-partial-width-windows
                        (if truncate-partial-width-windows nil t))))


;;----------------------------------------------------------------------------
;; Frame (window) config: fonts, colors, geometry, widgets, etc.
;;
;; Order matters on several of these statements.
;;
;; Override these in ~/.emacs for each host.
(defvar my-font     "Liberation Mono-10")
(defvar my-geometry '((width . 90) (height . 60) (top . 0) (left . -3)))

(if (and (boundp 'my-unix-p) my-unix-p)
    (progn
      (defun my-frame-create-hook (frame)
        (select-frame frame)
        (set-frame-font my-font)
        ;; Only set geometry on the first frame.  The length of frame-list is
        ;; always one more than the number of frames.
        (if (<= (length (frame-list)) 2)
            (modify-frame-parameters frame my-geometry))
      )
      (add-to-list 'after-make-frame-functions 'my-frame-create-hook t)
      ;; Only required before Emacs 23, but doesn't hurt anything in later
      ;; versions.
      (set-frame-font my-font)
    )
)

;; Only required under Windows or before Emacs 23, but doesn't hurt anything
;; in later versions.
(setq default-frame-alist my-geometry)

;; This func is helpful when I encounter an unreadable font face in the color
;; theme.
(defun my-unfontify ()
  "Interactive wrapper for font-lock-unfontify-buffer"
  (interactive)
  (font-lock-unfontify-buffer)
)

;; Don't blink the cursor.  The conditional is to support certain older
;; versions of Emacs.
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; I prefer "tooltips" to appear in the minibuffer instead of a separate X
;; window.  Also, when running under Linux and displaying on a Cygwin X server
;; (or Xming), tooltips appear even when Emacs is minimized.  (They appear
;; where they would if Emacs had not been minimized.)  I think this is a bug
;; in the Cygwin X server.
(if (>= emacs-major-version 21) (tooltip-mode 0))

;; The vertical scroll bars default to the left side.  (At least, an older
;; version of Emacs under Unix did at one point.)
(set-scroll-bar-mode 'right)

;; Hide pull-down menus.  I never use them, so they waste space.
(menu-bar-mode 0)

;; Hide annoying and space-wasting button bar.  The conditional is to support
;; certain older versions of Emacs.
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))

(load "my-color-theme")

;; Enable context-sensitive colors and fonts.
(global-font-lock-mode 1)
