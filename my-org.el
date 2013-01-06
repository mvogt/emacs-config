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
;; http://orgmode.org
;;
;; Move some org-mode keys to alternates that don't conflict with my preferred
;; global mappings.
;; This must be defined before initializing org-mode.
(setq org-disputed-keys '(([(shift up)]   . [(meta p)])
                          ([(shift down)] . [(meta n)])))
(setq org-replace-disputed-keys t)

(require 'org-install)

;; A little navigation help.  Set to 'reversed for opposite behavior.  Set to
;; nil (which is the default) to disable.
(setq org-special-ctrl-a/e t)

;; Blank all but the last star in a heading so it appears to be indented.
(setq org-hide-leading-stars t)

;; When opening a file, show it fully expanded.
(setq org-startup-folded nil)

(add-hook 'org-mode-hook
  (function (lambda ()
              ;; Match C-a / C-e
              (local-set-key [home] 'org-beginning-of-line)
              (local-set-key [end]  'org-end-of-line)
              ;; More familiar to me than M-S-left/right
              (local-set-key [?\M-,] 'org-promote-subtree)
              (local-set-key [?\M-.] 'org-demote-subtree)
            )
  )
)
