;;----------------------------------------------------------------------------
;; CUA Mode
;;
;; I wish CUA mode had a variable to disable use of the S-arrow keys for
;; region highlighting.  It's not really a problem because my S-up / S-down
;; mapping for vertical scrolling overrides it, and the feature is still handy
;; with C-S-right and C-S-left.

;; Don't let typing replace the selection.
(setq cua-delete-selection nil)

;; Remap minimize/background to the standard undo key (i.e. swap with undo).
(global-set-key [?\C-/] 'suspend-frame)

(cua-mode t)
