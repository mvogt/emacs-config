;;
;; Copyright 2020  Mark Vogt
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
;; org-mode
;; https://orgmode.org
;;

;; Move some Org mode keys to alternates that don't conflict with my preferred
;; global mappings.
;; This must be defined before initializing org-mode.
(setq org-disputed-keys '(([(shift up)]   . [?\M-\C-p])
                          ([(shift down)] . [?\M-\C-n])))
(setq org-replace-disputed-keys t)

;; A little navigation help.  Set to 'reversed for opposite behavior.  Set to
;; nil (which is the default) to disable.
(setq org-special-ctrl-a/e t)

;; Blank all but the last star in a heading so it appears to be indented.
(setq org-hide-leading-stars t)

;; When opening a file, show it fully expanded.
(setq org-startup-folded nil)

;; Add UUIDs to event entries in buffer when exporting to iCal.
(setq org-icalendar-store-UID t)

;; Don't create global index ~/.emacs.d/.org-id-locations of UUIDs generated
;; for org entries.
(setq org-id-track-globally nil)

;; In agenda mode, show a span of 10 days centered around today.
(setq org-agenda-start-day "-3d")
(setq org-agenda-span 11)

(require 'org)

;; Enable structure template expansion. For example <s TAB to create a source
;; code block.
(require 'org-tempo)

(add-hook 'org-mode-hook
  (function (lambda ()
              (toggle-truncate-lines 0)    ; enable line wrap
              (local-set-key [?\C-c ?\C-8] 'org-list-make-subtree)
              (local-set-key [?\C-c ?\C-6] 'org-up-element)
              (local-set-key [?\C-c ?w]    'my-copy-cur-line-val)
              (local-set-key [?\C-y]       'my-yank)
              (local-set-key [?\C-\']      'other-window)
              (local-set-key [?\M-h]
                             (lambda () (interactive)
                               (move-to-window-line 0)))
              ;; Match C-a / C-e
              (local-set-key [home]        'org-beginning-of-line)
              (local-set-key [end]         'org-end-of-line)
            )
  )
)

;; Restore defaults nuked by my "disputed keys" setting above.
;; I wish that setting didn't affect minibuffer keys, but it does.
(define-key org-read-date-minibuffer-local-map [(shift up)]
  (lambda () (interactive)
    (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map [(shift down)]
  (lambda () (interactive)
    (org-eval-in-calendar '(calendar-forward-week 1))))

;; Shorten the default list of holidays to ones I want.
(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays
              holiday-other-holidays holiday-christian-holidays
              holiday-solar-holidays))
