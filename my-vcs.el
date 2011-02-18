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
;; Can't use autoload because of my custom function below.
(when (file-accessible-directory-p my-3rd-party-elisp-path)
  (require 'magit)
  (define-key magit-status-mode-map [?\C-c ?\C-d] 'my-magit-difftool-item)
)

(defun my-magit-difftool-item ()
  "Launch difftool on the item at point."
  (interactive)
  (magit-section-action (item info "stage")
    ((untracked *)
     (error "Can't run difftool on untracked file"))
    ((unstaged diff)
     (start-process "git difftool" nil "git" "difftool"
                    (magit-diff-item-file item)))
    ((unstaged *)
     (error "Must select all of a single file to run difftool"))
    ((staged diff)
     (start-process "git difftool" nil "git" "difftool" "--cached"
                    (magit-diff-item-file item)))
    ((staged *)
     (error "Must select all of a single file to run difftool"))
    ((hunk)
     (error "Can't diff this hunk"))
    ((diff)
     (error "Can't diff this (FIXME: What is it?)"))
  )
)

;; Redefines standard function to fix a bug and change the recenter behavior
;; to my liking.
(defun magit-goto-next-section ()
  "Go to the next magit section."
  (interactive)
  (let* ((section (magit-current-section))
	 (next (or (and (not (magit-section-hidden section))
			(magit-section-children section)
			(magit-find-section-after (point)
						  (magit-section-children
						   section)))
		   (magit-next-section section))))

    (if next
	(progn
	  (goto-char (magit-section-beginning next))
	  (if (memq magit-submode '(log reflog))
	      (magit-show-commit next))
	  (if (not (magit-section-hidden next))
	      (let ((offset (- (line-number-at-pos
				(magit-section-beginning next))
			       (line-number-at-pos
				(magit-section-end next)))))
                (unless (pos-visible-in-window-p (magit-section-end next))
                  ;; Bug in original: missing abs
                  (if (< (abs offset) (window-height))
                      (recenter offset)
                    (recenter 0)
                  )
                )
              )
          )
        )
      (message "No next section")
    )
  )
)

(define-key vc-prefix-map [?x] 'magit-status)
(define-key vc-prefix-map [?k] (lambda () (interactive)
                                 (start-process "Git GUI" nil "git" "gui")))
(define-key vc-prefix-map [?K] (lambda () (interactive)
                                 (start-process "gitk" nil "gitk" "--all")))
