;;----------------------------------------------------------------------------
;; Minibuffer completion
;;
;; In the pop-up completions buffer, show case-insensitive matches.
(setq read-file-name-completion-ignore-case t)
;; ...and don't print boilerplate text at the top.
(setq completion-show-help nil)

;; Makes default completion tolerable until I learn ido or icicles.
(defun my-toggle-completions-window ()
  "Toggle between the completions window and the minibuffer.
When switching back to the minibuffer, quit the completions window to show the
buffer that was there before."
  (interactive)
  (if (eq (get-buffer-window) (get-buffer-window "*Completions*"))
      (progn
        (quit-window)
        (select-window (minibuffer-window))
      )
    (switch-to-completions)
  )
)

(define-key minibuffer-local-map     [backtab] 'my-toggle-completions-window)
(define-key completion-list-mode-map [backtab] 'my-toggle-completions-window)
(define-key completion-list-mode-map [?q]      'my-toggle-completions-window)
(define-key completion-list-mode-map [?\C-g]   'my-toggle-completions-window)
