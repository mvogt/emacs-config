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

;; Default buffer selection max window height is 20, and I sometimes have more
;; than 20 buffers open and want to see them all.
(setq bs-max-window-height 40)

;; Add another, more familiar key to bs-show mode.
(define-key bs-mode-map [?1]       'bs-select-in-one-window)

;; Additional mappings for easier alternate hand operation.
(define-key bs-mode-map [C-return] 'bs-select-other-window)
(define-key bs-mode-map [?\C-j]    'bs-select-other-window)
(define-key bs-mode-map [?\M-o]    'bs-tmp-select-other-window)

;; Additional custom funcs.
(define-key bs-mode-map [?i]       'my-bs-vert-select-other-window)
(define-key bs-mode-map [?l]       'my-bs-horiz-select-other-window)

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

;; Alternative to bs-view in bs.el that doesn't try to visit the buffer inside
;; the tiny buffer selection window.
(defun my-bs-view ()
  "View current line's buffer in View mode, and leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (view-buffer buffer)
  )
)

(defun my-bs-vert-select-other-window ()
  "Split the original window vertically, and make the other window select this
line's buffer."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    ;; Similar hack to my-equal-window-split-vert
    (split-window nil (+ 2 (/ (window-width) 2)) t)
    (other-window 1)
    (switch-to-buffer buffer)
  )
)

(defun my-bs-horiz-select-other-window ()
  "Split the original window horizontally, and make the other window select
this line's buffer."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (bs--restore-window-config)
    (split-window nil (/ (window-height) 2))
    (other-window 1)
    (switch-to-buffer buffer)
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

;; Alternative to bs-select in bs.el that adds 2D tiling of windows.
(defun my-bs-select ()
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

(defun bury-buffer-if-dupe ()
  "Bury the current buffer if it's also displayed in another window.
Also bury if it's the bs-show menu."
  (interactive)
  (let ((cur-win-buf (window-buffer)))
    (when (or (< 1 (length (get-buffer-window-list cur-win-buf 0)))
              (equal (buffer-name cur-win-buf) "*buffer-selection*"))
      (bury-buffer)
      ;; I don't loop here because I couldn't think of an algorithm that
      ;; covers all possible cases. There could be a window for every buffer,
      ;; so looping until we get one that's not displayed would loop forever.
      ;; I only handle the two most undesirable common cases: killing a buffer
      ;; shows a new one already displayed, and then burying that one shows
      ;; the bs-show menu buffer.
      (if (equal (buffer-name (window-buffer)) "*buffer-selection*")
          (bury-buffer))
    )
  )
)

(defun my-bs-delete ()
"Wrapper for bs-delete that conditionally buries the newly displayed buffer."
  (interactive)
  (let* ((victim (bs--current-buffer))
         (vic-windows (get-buffer-window-list victim 0))
         (sole-vic-window (if (= 1 (length vic-windows))
                              (car vic-windows)
                            nil)))
    (call-interactively 'bs-delete)
    (when sole-vic-window
      (save-selected-window
        (select-window sole-vic-window)
        (bury-buffer-if-dupe)
      )
    )
  )
)

(defun my-kill-buffer ()
  "Wrapper for kill-buffer that conditionally buries the new buffer."
  (interactive)
  (kill-buffer)
  (bury-buffer-if-dupe)
)

(defun my-bury-buffer-other-window ()
  "Bury the buffer in the other window."
  (interactive)
  (save-selected-window
    (other-window 1)
    (bury-buffer)
  )
)

(substitute-key-definition 'bs-select 'my-bs-select bs-mode-map)
(substitute-key-definition 'bs-view   'my-bs-view   bs-mode-map)
(substitute-key-definition 'bs-delete 'my-bs-delete bs-mode-map)

(global-set-key [?\C-x ?b]    'rename-buffer)
(global-set-key [f10]         'delete-window)
(global-set-key [?\M-\"]      'delete-window)
(global-set-key [f11]         'delete-other-windows)
(global-set-key [?\M-\]]      'delete-other-windows)
(global-set-key [f12]         'other-window)
(global-set-key [?\M-\']      'other-window)
(global-set-key [C-f12]       'my-bury-buffer-other-window)
(global-set-key [?\C-x ?9]    'my-bury-buffer-other-window)
(global-set-key [?\M-z]       'my-kill-buffer)
