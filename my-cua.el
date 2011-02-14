;;----------------------------------------------------------------------------
;; CUA Mode
;;
;; I wish CUA mode had a variable to disable use of the S-arrow keys for
;; region highlighting.  It's not really a problem because my S-up / S-down
;; mapping for vertical scrolling overrides it, and the feature is still handy
;; with C-S-right and C-S-left.

;; Don't acutally use the CUA keys.  I want this mode just for the rectangles.
(setq cua-enable-cua-keys nil)

;; Remap minimize/background to the standard undo key (i.e. swap with undo).
;;(global-set-key [?\C-/] 'suspend-frame)

;; Don't let typing replace the selection.
(setq cua-delete-selection nil)

(setq cua-auto-tabify-rectangles nil)

;; Don't use C-return for rectangle mode because it interferes with some
;; custom mappings in minor modes that I use.
(setq cua-rectangle-mark-key [?\C-x ?r ?v])
(global-set-key [?\C-x ?r ?v] 'cua-set-rectangle-mark)

(cua-mode t)
