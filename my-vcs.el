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
(autoload 'magit-status "magit")

(defvar my-gitk-load-limit 10000)
(setq magit-gitk-load-limit my-gitk-load-limit)

(defun my-launch-gitk (arg)
  "Launch gitk showing all branches.
With a prefix, don't limit the number of commits loaded to
my-gitk-load-limit."
  (interactive "P")
  (if arg
      (start-process "gitk" nil "gitk" "--all")
    (start-process "gitk" nil "gitk"
                   (format "--max-count=%d" my-gitk-load-limit) "--all")
  )
)

(when (file-accessible-directory-p my-3rd-party-elisp-path)
  (define-key vc-prefix-map [?x] (lambda () (interactive)
                                   (magit-status default-directory)
                                   (require 'magit-classic-theme)))
  (define-key vc-prefix-map [?n] (lambda () (interactive)
                                   (magit-status default-directory)
                                   (require 'magit-classic-theme)
                                   (require 'magit-svn)
                                   (magit-svn-mode 1)
                                   (magit-refresh)))
)
(define-key vc-prefix-map [?k] (lambda () (interactive)
                                 (start-process "Git GUI" nil "git" "gui")))
(define-key vc-prefix-map [?K] 'my-launch-gitk)
