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
;; Move some Org mode keys to alternates that don't conflict with my preferred
;; global mappings.
;; This must be defined before initializing org-mode.
(setq org-disputed-keys '(([(shift up)]   . [(meta p)])
                          ([(shift down)] . [(meta n)])))
(setq org-replace-disputed-keys t)

(require 'org-install)
;; Can't use autoload for the big file because I'm overriding some funcs
;; below.
(require 'org)

;; Customized: Move to previous paragraph when at first item or when a prefix
;; is given.
(defun org-previous-item (&optional arg)
  "Move to the beginning of the previous item.
Throw an error when not in a list. Move to previous paragraph when at
first item, unless `org-list-use-circular-motion' is non-nil."
  (interactive "P")
  (if arg
      (backward-paragraph)
    (let ((item (org-in-item-p))
          (saved-pos (point)))
      (if (not item)
          (error "Not in an item")
        (goto-char item)
        (let* ((struct (org-list-struct))
               (prevs (org-list-prevs-alist struct))
               (prevp (org-list-get-prev-item item struct prevs)))
          (cond
           (prevp (goto-char prevp))
           (org-list-use-circular-motion
            (goto-char (org-list-get-last-item item struct prevs)))
           (t
            (goto-char saved-pos)
            (backward-paragraph))
          )
        )
      )
    )
  )
)

;; Customized: Move to next paragraph when at last item or when a prefix is
;; given.
(defun org-next-item (&optional arg)
  "Move to the beginning of the next item.
Throw an error when not in a list. Move to next paragraph when at
last item, unless `org-list-use-circular-motion' is non-nil."
  (interactive "P")
  (if arg
      (forward-paragraph)
    (let ((item (org-in-item-p))
          (saved-pos (point)))
      (if (not item)
          (error "Not in an item")
        (goto-char item)
        (let* ((struct (org-list-struct))
               (prevs (org-list-prevs-alist struct))
               (nextp (org-list-get-next-item item struct prevs)))
          (cond
           (nextp (goto-char nextp))
           (org-list-use-circular-motion
            (goto-char (org-list-get-first-item item struct prevs)))
           (t
            (goto-char saved-pos)
            (forward-paragraph))
          )
        )
      )
    )
  )
)

;; Customized for me by not messing with headline priority and by falling back
;; to paragraph movement instead of throwing an error. Also fixes a minor bug
;; as noted below.
(defun org-shiftup (&optional arg)
  "Increase item in timestamp or move to previous item or paragraph.
Calls `org-timestamp-up' or `org-previous-item' or `backward-paragraph',
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ;; Changed from org-at-item-p to org-in-item-p because the former is only
   ;; true on the first line of a list item.
   ((and (not org-support-shift-select) (org-in-item-p))
    (call-interactively 'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (backward-paragraph))
  )
)

;; Same changes as org-shiftup.
(defun org-shiftdown (&optional arg)
  "Decrease item in timestamp or move to next item or paragraph.
Calls `org-timestamp-down' or `org-next-item' or `forward-paragraph'
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ;; Changed from org-at-item-p to org-in-item-p because the former is only
   ;; true on the first line of a list item.
   ((and (not org-support-shift-select) (org-in-item-p))
    (call-interactively 'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (forward-paragraph))
  )
)

;; A little navigation help.  Set to 'reversed for opposite behavior.  Set to
;; nil (which is the default) to disable.
(setq org-special-ctrl-a/e t)

;; Blank all but the last star in a heading so it appears to be indented.
(setq org-hide-leading-stars t)

;; When opening a file, show it fully expanded.
(setq org-startup-folded nil)

(setq org-edit-timestamp-down-means-later t)

(add-hook 'org-mode-hook
  (function (lambda ()
              ;; Match C-a / C-e
              (local-set-key [home] 'org-beginning-of-line)
              (local-set-key [end]  'org-end-of-line)
            )
  )
)
