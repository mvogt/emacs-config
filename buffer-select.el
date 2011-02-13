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
;; Buffer selection
;;
;; "Buffer Show" mode.  Don't bother with attempting to autoload just the
;; parts I need to redefine.
(load "bs")

;; In the default config, when showing all buffers, the internal ones like
;; shell command output get buried at the end -- even if they're current.  I
;; don't like that because it requires pressing more keys to navigate around.
(setq bs-default-configuration "all")

;; Add another, more familiar key to bs-show mode.
(define-key bs-mode-map [?1]       'bs-select-in-one-window)

;; Additional mappings for easier alternate hand operation.
(define-key bs-mode-map [C-return] 'bs-select-other-window)
(define-key bs-mode-map [?\C-j]    'bs-select-other-window)
(define-key bs-mode-map [?\M-o]    'bs-tmp-select-other-window)

;; Redefine this func from bs.el so that the other window doesn't disappear
;; when exiting bs-show mode.
(defun bs-tmp-select-other-window ()
  "Make the other window select this line's buffer.
The current window remains selected."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (display-buffer buffer t)
    (bs--show-with-configuration bs-current-configuration)
  )
)

(defun my-equal-window-split-vert (num-windows)
  (delete-other-windows)
  (let ((width (/ (frame-width) num-windows)))
    (while (> num-windows 1)  ; 1 instead of 0 avoids fence post error
      ;; The width adjustment here is a hack to get as close as possible to
      ;; equal-sized windows.  Emacs's split-window func does some math behind
      ;; the scenes to handle the width of the vertical divider, and I
      ;; couldn't fully reverse engineer it.
      (split-window nil (+ width (if (> num-windows 2) 1 2)) t)
      (other-window 1)
      (setq num-windows (1- num-windows))
    )
  )
  (other-window 1)  ; back to starting window
)

(defun my-equal-window-split-horiz (num-windows)
  (let ((height (/ (frame-height) num-windows)))
    (while (> num-windows 1)  ; 1 instead of 0 avoids fence post error
      (split-window nil height)
      (other-window 1)
      (setq num-windows (1- num-windows))
    )
  )
)

;; Divide up the current frame into a 2D tile of num-widows.
(defun my-tile-windows (num-windows)
  (let* ((num-columns (min num-windows
                           (/ (frame-width) (/ split-width-threshold 2))))
         (min-rows (/ num-windows num-columns))
         (remaining-rows (% num-windows num-columns))
         cur-rows)
    (my-equal-window-split-vert num-columns)
    ;; Start the horizontal splits one column of windows to the right so that
    ;; the left column will have the fewest possible horizontal splits and
    ;; wind up with the caller's selected buffer.
    (other-window 1)
    (while (> num-columns 0)
      (setq cur-rows min-rows)
      (unless (= remaining-rows 0)
        (setq cur-rows (1+ cur-rows))
        (setq remaining-rows (1- remaining-rows))
      )
      (my-equal-window-split-horiz cur-rows)
      ;; Advance to the start of the next column of windows unless this is the
      ;; last column.  The special case ensures the caller's selected buffer
      ;; gets the largest possible window.
      (if (> num-columns 1) (other-window 1))
      (setq num-columns (1- num-columns))
    )
  )
)

;; Redefine this func from bs.el to add 2D tiling of windows.
(defun bs-select ()
  "Select current line's buffer and other marked buffers.
If there are no marked buffers the window configuration before starting
Buffer Selection Menu will be restored.
If there are marked buffers each marked buffer and the current line's buffer
will be selected in a window.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (switch-to-buffer buffer)
    (when bs--marked-buffers
      ;; More than one buffer has been selected.  Tile them.
      ;; In case the selected buffer was also marked, we delete it from the
      ;; list of marked buffers and prepend it to our working list.
      (let ((all (delq buffer bs--marked-buffers)))
        (add-to-list 'all buffer)
        (my-tile-windows (length all))
        (dolist (buf all)
          (switch-to-buffer buf)
          (other-window 1)
        )
      )
    )
  )
)

(defun my-bury-buffer-other-window ()
  "Bury the buffer in the other window."
  (interactive)
  (let ((window (get-buffer-window)))
    (other-window 1)
    (bury-buffer)
    (select-window window)
  )
)

;; Competitor to bs-show and buffer-menu.  Remaps C-x b.
(iswitchb-mode 1)

(global-set-key [?\C-x ?b]    'rename-buffer)
(global-set-key [?\C-x ?\C-b] 'iswitchb-buffer)
(global-set-key [?\C-x ?\M-b] 'iswitchb-buffer)
(global-set-key [?\C-`]       'bs-show)
(global-set-key [?\M-j]       'bs-show)
(global-set-key [f10]         'delete-window)
(global-set-key [f11]         'delete-other-windows)
(global-set-key [f12]         'other-window)
(global-set-key [C-f12]       'my-bury-buffer-other-window)
(global-set-key [?\C-x ?9]    'my-bury-buffer-other-window)
(global-set-key [?\M-z]       (lambda () (interactive)
                                (kill-buffer (buffer-name))))
