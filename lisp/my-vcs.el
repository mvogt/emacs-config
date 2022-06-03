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

;; For some reason, starting in Emacs 25.2 and/or Magit 20190906.2217, my
;; function overrides below don't take effect unless I first manually load the
;; file where their original definitions live.
(load "magit-extras")

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

(setq magit-branch-direct-configure nil)
(setq magit-log-show-refname-after-summary nil)
(setq magit-diff-refine-hunk t)
(setq magit-log-section-commit-count 0)
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

;; Trailing asterisk changes the name's color is bs-show.
;; Not sure why that's not the default.
(setq magit-buffer-name-format "*%M%v: %t*")

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


;;
;; Helper functions for quickly selecting and grabbing the text of a git or
;; svn branch
;;

(defvar my-vcs-branch-hist nil
  "History list for the prompt in `my-get-branch'")
(defvar my-vcs-branches nil
  "List of string names for branches used by `my-get-branch'.
On every call to that function, it runs a script that regenerates
the value of this variable.")
(defvar my-svn-branch-lister "~/.emacs.d/ls-svn-branches.sh"
  "Script to generate ELisp code defining my-vcs-branches for svn.")
(defvar my-git-branch-lister "~/.emacs.d/ls-git-branches.sh"
  "Script to generate ELisp code defining my-vcs-branches for git.")

(defun my-get-branch (type-name lister-scr)
  "Get branch name string with completion, and return it."
  (let ((buf (generate-new-buffer "*my-branch-lister*")))
    (if (not (= 0 (call-process lister-scr nil buf)))
        (message "%s failed" lister-scr)
      (eval-buffer buf)
      (kill-buffer buf)
      (completing-read (format "%s branch: " type-name)
                       my-vcs-branches nil nil nil 'my-vcs-branch-hist)
    )
  )
)

(defhydra my-branch-hydra (:color blue)
  "
Get VCS branch:
_g_ git, add to kill ring   _s_ Subversion, add to kill ring
_G_ git, insert in buffer   _S_ Subversion, insert in buffer
"
  ("g" (kill-new (my-get-branch "git" my-git-branch-lister)) nil)
  ("G" (insert (my-get-branch "git" my-git-branch-lister)) nil)
  ("s" (kill-new (my-get-branch "Subversion" my-svn-branch-lister)) nil)
  ("S" (insert (my-get-branch "Subversion" my-svn-branch-lister)) nil)
)

(global-set-key (kbd "M-<f5>")    'my-branch-hydra/body)
(global-set-key (kbd "<ESC><f5>") 'my-branch-hydra/body) ; for text console
;; Extra binding for MacOS because of the infernal touch bar.
(global-set-key [?\M-g ?\M-5]     'my-branch-hydra/body)
