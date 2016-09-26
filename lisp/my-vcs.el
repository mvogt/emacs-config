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
;; Version control
;;
(defvar my-gitk-cmd "gitk")
(defvar my-gitk-load-limit 10000)

(defun my-launch-gitk (arg)
  "Launch gitk showing all branches.
With a prefix, don't limit the number of commits loaded to
my-gitk-load-limit."
  (interactive "P")
  (if arg
      (start-process "gitk" nil my-gitk-cmd "--all")
    (start-process "gitk" nil my-gitk-cmd
                   (format "--max-count=%d" my-gitk-load-limit) "--all")
  )
)

;; Override magit's functions for launching gitk. I always want to run it with
;; --all --max-count.
(defun magit-run-gitk ()
  (interactive)
  (my-launch-gitk nil)
)
(defun magit-run-gitk-all ()
  (interactive)
  (my-launch-gitk nil)
)

(define-key vc-prefix-map [?x] 'magit-status)
(define-key vc-prefix-map [?k] (lambda () (interactive)
                                 (start-process "Git GUI" nil "git" "gui")))
(define-key vc-prefix-map [?K] 'my-launch-gitk)

(setq magit-popup-show-common-commands nil)
(setq magit-branch-popup-show-variables nil)
(setq magit-log-show-margin nil)
(setq magit-log-show-refname-after-summary nil)
(setq magit-diff-refine-hunk t)
(setq git-commit-summary-max-length 78)
(add-hook 'magit-mode-hook
  (function (lambda ()
              ;; Unmap magit's override of my custom M-w behavior when the
              ;; region is inactive. They want to run
              ;; magit-copy-buffer-revision.
              (define-key magit-mode-map [?\M-w] nil)

              ;; There must be a cleaner way to do these besides using one
              ;; hook to remove another.

              ;; Disable auto-fill in commit message editing mode.
              (remove-hook 'git-commit-setup-hook
                           'git-commit-turn-on-auto-fill)

              ;; Don't show diff in buffer next to commit message edit buffer.
              ;; In gits with a symlinked .git dir, it doesn't even work
              ;; correctly.
              (remove-hook 'server-switch-hook 'magit-commit-diff)
            )
  )
  t
)

;; Careful: You can have only one call to custom-set-faces in all your startup
;; files.
(custom-set-faces
   ;; I want no special background for added or removed lines.
   ;; Background, shade of orange, and shade of green copied from my
   ;; customized theme.
   '(magit-diff-added ((t (:background "#1d1f21" :foreground "#b5bd68"))))
   '(magit-diff-removed ((t (:background "#1d1f21" :foreground "#de935f"))))
   ;; Keep foreground same as above, and copy background from
   ;; magit-diff-context-highlight.
   '(magit-diff-added-highlight ((t (:background "grey20" :foreground "#b5bd68"))))
   '(magit-diff-removed-highlight ((t (:background "grey20" :foreground "#de935f"))))
)

;; This hook just seems to slow me down.
(remove-hook 'find-file-hooks 'vc-find-file-hook)
