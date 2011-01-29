;;----------------------------------------------------------------------------
;; Cut/copy/paste, kill/yank
;;
;; When manipulating the kill ring, always set the X clipboard to the last
;; entry in the kill ring.  By default, only the X primary selection is
;; touched.
;;
;; Note: The following related functions only affect the clipboard and not the
;; primary selection: clipboard-kill-ring-save, clipboard-kill-region,
;; clipboard-yank.
;; See also: http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)

;; Highlight region selected and keep region available when not highlighted.
(transient-mark-mode t)
(setq mark-even-if-inactive t)

;; Setting the mark with a prefix will jump to a previous mark, cycling a ring
;; of all such previous marks.  With this variable set, you don't have to
;; supply a prefix for immediate subsequent set-mark commands.
(setq set-mark-command-repeat-pop t)

(defun my-kill-region (start end &optional user-register)
  "Call kill-region and set-register.  Without a prefix, use register #0.
With a numeric prefix, use that register number."
  ;; Actually, registers are indexed by character (i.e. numbers 0 - 255) and
  ;; not number.  So, if you answer '3' when prompted by the functions
  ;; copy-to-register or insert-register, they're actually operating on
  ;; register 51.  In order to stay compatible with those two built-in
  ;; functions, we limit the number of supported registers in this function to
  ;; 10 (0 - 9) and add the value to 48 (the decimal ASCII value for '0', of
  ;; course).
  (interactive "r\nP")
  (set-register (+ 48 (if (null user-register) 0 (mod user-register 10)))
                (filter-buffer-substring start end))
  (kill-region start end)
)

(defun my-kill-ring-save (start end &optional user-register)
  "Call kill-ring-save and set-register.  Without a prefix, use register #0.
With a numeric prefix, use that register number."
  ;; The comment about register numbers in func my-kill-region above also
  ;; applies here.
  (interactive "r\nP")
  (set-register (+ 48 (if (null user-register) 0 (mod user-register 10)))
                (filter-buffer-substring start end))
  (kill-ring-save start end)
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

;; For setting the mark, Meta-space feels more natural to me, and F3 is
;; much more convenient.
(global-set-key [f3]    'set-mark-command)
(global-set-key [?\M- ] 'set-mark-command)

(global-set-key [?\C-w] 'my-kill-region)
(global-set-key [?\M-w] 'my-kill-ring-save)
(global-set-key [?\C-y] 'my-yank)
