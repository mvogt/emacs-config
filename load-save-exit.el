;;----------------------------------------------------------------------------
;; Load, save, exit
;;
;; I often hit this key combination accidentally.
(setq confirm-kill-emacs 'y-or-n-p)

;; Docs say to load browse-url before ffap.  I don't care about poor startup
;; time, and I use one ffap func that isn't declared with autoload.
(load "browse-url")
(load "ffap")
;; Activate special behavior with prefix instead of by default.
(setq ffap-require-prefix t)

;; ffap is much better than plain old find-file.
(global-set-key [?\C-x ?\C-f]     'find-file-at-point)
(global-set-key (kbd "M-<f2>")    'find-file-at-point)
(global-set-key (kbd "<ESC><f2>") 'find-file-at-point) ; for text console
(global-set-key (kbd "M-<f1>")    'ffap-other-window)
(global-set-key (kbd "<ESC><f1>") 'ffap-other-window)  ; for text console

;; The interactive prompt of save-some-buffers is annoying, and I sometimes
;; accidentally hit this, its default key sequence.
(global-unset-key [?\C-x ?s])

(global-set-key [f2]          'save-buffer)
(global-set-key [?\C-x ?z]    (lambda () (interactive)
                                (save-some-buffers t)))
(global-set-key [?\C-x ?\C-z] (lambda () (interactive)
                                (save-some-buffers t)
                                (save-buffers-kill-emacs)))

;; The Emacs 23 daemon feature changes this to save-buffers-kill-terminal.
;; Put it back so I can kill the daemon with the usual exit sequence.
(global-set-key [?\C-x ?\C-c] 'save-buffers-kill-emacs)
