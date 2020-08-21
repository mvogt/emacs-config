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
;; Cut/copy/paste, kill/yank
;;
;; Highlight region selected and keep region available when not highlighted.
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;; Setting the mark with a prefix will jump to a previous mark, cycling a ring
;; of all such previous marks.  With this variable set, you don't have to
;; supply a prefix for immediate subsequent set-mark commands.
(setq set-mark-command-repeat-pop t)


(defun my-cut-or-copy (cut-p user-register)
  "Worker function for my-kill-region and my-kill-ring-save.
If cut-p is nil, copy, else cut."
  ;; Note: Registers are indexed by character (i.e. numbers 0 - 255) and not
  ;; number. So, if you answer '3' when prompted by the functions
  ;; copy-to-register or insert-register, they're actually operating on
  ;; register 51. In order to stay compatible with those two built-in
  ;; functions, we limit the number of supported registers in this function to
  ;; 10 (0 - 9) and add the value to 48 (the decimal ASCII value for '0', of
  ;; course).
  (let (start end)
    (if (and (mark t) (use-region-p))
        (progn
          (setq start (min (mark) (point)))
          (setq end (max (mark) (point)))
        )
      (setq start (line-beginning-position))
      (setq end (line-beginning-position 2))
    )
    (set-register (+ 48 (if (null user-register) 0 (mod user-register 10)))
                  (filter-buffer-substring start end))
    (if cut-p
        (kill-region start end)
      (kill-ring-save start end)
      (unless (and (mark t) (use-region-p))
        (forward-line)
        (setq this-command 'kill-region)
      )
    )
  )
)

(defun my-kill-region (&optional user-register)
  "Call kill-region and set-register.  Without a prefix, use register #0.
With a numeric prefix, use that register number.
If the region is inactive, operate on the current line."
  (interactive "P")
  (my-cut-or-copy t user-register)
)

(defun my-kill-ring-save (&optional user-register)
  "Call kill-ring-save and set-register.  Without a prefix, use register #0.
With a numeric prefix, use that register number.
If the region is inactive, operate on the current line, move to the next one,
and cause any following kill command to append the previous kill."
  (interactive "P")
  (my-cut-or-copy nil user-register)
)

(defun my-yank (&optional register)
  "Without a prefix, just call yank.  With a numeric prefix, call
insert-register with the prefix as the register number."
  ;; By wrapping yank like this, we remove it's prefix arg features.  Those
  ;; are: a C-u prefix swaps point and mark after inserting, and a numeric
  ;; prefix yanks that buffer index from the kill ring.  Those are not useful
  ;; to me, so no great loss there.  I much prefer having a numeric prefix
  ;; refer to a register.
  (interactive "P")
  (if (null register)
      (yank)
    (insert-register (+ 48 (mod register 10)) t)
  )
)

(defun my-yank-scratch (&optional register)
  "Same as my-yank but into a new 'wip' buffer.
Also, if the universal argument is given, don't yank anything."
  (interactive "P")
  (switch-to-buffer (generate-new-buffer "wip"))
  (text-mode)
  (or (consp register) (my-yank register))
)

;; For setting the mark, Meta-space feels more natural to me, and F3 is
;; much more convenient.
(global-set-key [f3]          'set-mark-command)
(global-set-key [?\M- ]       'set-mark-command)

(global-set-key [?\C-w]       'my-kill-region)
(global-set-key [?\M-w]       'my-kill-ring-save)
(global-set-key [?\C-y]       'my-yank)
(global-set-key [?\C-x ?\C-y] 'my-yank-scratch)
(global-set-key [?\C-x ?\M-y] 'my-yank-scratch)
