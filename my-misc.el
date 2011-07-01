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
;; Miscellaneous
;;
;; I found this trick in cua-base.el:cua--prefix-override-replay.
;; It's useful for aliasing prefixes other than C-x.  (The method I used for
;; aliasing C-x doesn't work for C-c.  It's probably because C-c is treated
;; specially by major and minor modes.)  This hack is the only solution I
;; could find.
(defun my-replay-prefix-key (arg key)
  "Replace the current key in the input event queue with the specified key."
  (setq this-command last-command)    ; don't record this command
  (setq prefix-arg arg)               ; restore the prefix arg
  (reset-this-command-lengths)
  ;; Insert the key at the front of the event queue.
  (setq unread-command-events (cons key unread-command-events))
)

(defun insert-timestamp (detailed-p)
  "Insert a time stamp in my preferred style.
With a prefix, Insert a time stamp that a spreadsheet understands."
  (interactive "P")
  (insert (format-time-string (if detailed-p "%m/%d/%Y %H:%M" "%Y%m%d")
                              (current-time)))
)

(defun insert-fixme (str-type)
  "Insert a string of the form '<keyword>-<user>-<date>: '.
<keyword> is selected by a single character response to a prompt,
and it can be: FIXME, NOTE, HACK.
<user> is the current user name.
<date> is a time stamp of the form yyyymmdd."
  (interactive "cInsert [f]ixme (default), [n]ote, [h]ack, or [d]ebug?")
  (insert (format "%s-%s-%s: "
                  (if (= str-type ?n) "NOTE"
                    (if (= str-type ?h) "HACK"
                      (if (= str-type ?d) "DEBUG" "FIXME")))
                  (user-login-name)
                  (format-time-string "%Y%m%d" (current-time))))
)

(defun my-format-search-url (terms)
  (concat "https://duckduckgo.com/?q="
          (replace-regexp-in-string
           "\\s +" "+"
           (replace-regexp-in-string "+" "%2B" terms)))
)

(defvar my-jira-base-url "https://sandbox.onjira.com/browse/")

(autoload 'browse-url "browse-url")
(require 'my-cur-word-or-region "grep-compile")
(defun my-browse-url ()
  "Wrapper for browse-url.  Prompts for URL, search, or Jira issue.
Defaults to the word around the point or the active region as the default.
Search terms are passed as a single string to func my-format-search-url,
which should return a URL string."
  (interactive)
  (message "[U]RL, [S]earch, or [J]ira?")
  (let ((which-func (read-char)))
    (cond
     ((= which-func ?u)
      (call-interactively 'browse-url))
     ((= which-func ?s)
      (browse-url (my-format-search-url
                   (read-string "Search terms: " (my-cur-word-or-region)))))
     ((= which-func ?j)
      (browse-url (concat my-jira-base-url
                          (read-string "Jira issue: "
                                       (my-cur-word-or-region)))))
    )
  )
)

;; (format "%c" val) is adequate but not as fancy as (single-key-description).
(require 'my-cur-word-or-region "grep-compile")
(defun my-describe-key ()
  "Interactive wrapper for single-key-description that defaults
to the decimal value at the point or region."
  (interactive)
  (let ((val (read-number "Decimal value of character: "
                          (string-to-number (my-cur-word-or-region)))))
    (message "Value %d (decimal) = character %s"
             val (single-key-description val))
  )
)

(defun my-man-cleanup ()
  "Convert current buffer into a manual page.
Cleans up ANSI formatting chars."
  (interactive)
  (Man-fontify-manpage)
  (Man-cleanup-manpage)
  (Man-mode)
)


;;
;; Actual prefix key maps are the proper Emacs solution, but I want a menu of
;; all my choices.  I guess I could customize the pull-down menus, but those
;; waste screen real estate, and I've had them disabled for years.
;;

(defun my-prefix-menu-modes ()
  "Display a shortcut menu in the minibuffer of my favorite mode
enable functions that I don't use often enough to bind to keys of
their own."
  (interactive)
  (message "Mode [d]iff, [t]ext, [h]tml, [s]hell, [c]++, [m]ake,\n[e]macs lisp, lisp [i]nteraction, [l]ine numbers, [w]hitespace?")
  (let ((which-func (read-char)))
    (cond
     ((= which-func ?d) (diff-mode))
     ((= which-func ?t) (text-mode))
     ((= which-func ?h) (html-mode))
     ((= which-func ?s) (sh-mode))
     ((= which-func ?c) (c++-mode))
     ((= which-func ?m) (makefile-mode))
     ((= which-func ?e) (emacs-lisp-mode))
     ((= which-func ?i) (lisp-interaction-mode))
     ((= which-func ?l) (linum-mode))
     ((= which-func ?w) (whitespace-mode))
    )
  )
)

(defun my-prefix-menu-misc ()
  "Display a shortcut menu in the minibuffer of miscellaneous
functions that I don't use often enough to bind to keys of their
own."
  (interactive)
  (message "Call [e]val buf, eval [r]egion, cmd [h]ist, browse [k]ill ring,\n[m]an cleanup, [u]nfontify, describe [c]har, describe ke[y],\n[g]db many windows, i[s]earch fwd word, re[n]ame uniquely,\n[l]ist colors, read col[o]r, customize [f]ace?")
  (let ((which-func (read-char)))
    (cond
     ((= which-func ?e) (eval-buffer))
     ((= which-func ?r) (eval-region (point) (mark)))
     ((= which-func ?h) (describe-variable 'command-history))
     ((= which-func ?k) (browse-kill-ring))
     ((= which-func ?m) (my-man-cleanup))
     ((= which-func ?u) (my-unfontify))
     ((= which-func ?c) (describe-char))
     ((= which-func ?y) (my-describe-key))
     ((= which-func ?g) (gdb-many-windows))
     ((= which-func ?s) (isearch-forward-word))
     ((= which-func ?n) (rename-uniquely))
     ((= which-func ?l) (list-colors-display))
     ((= which-func ?o) (read-color))
     ((= which-func ?f) (customize-face))
    )
  )
)


(add-hook 'diff-mode-hook
  (function (lambda ()
              (whitespace-mode 1))
  )
)

;; I never use the default upcase-word binding of M-u.  It's much more useful
;; to me as the universal prefix because it allows me to hold down Alt for the
;; entirety of many key sequences.
(global-set-key [?\M-u] 'universal-argument)
;; Ensure multiple M-u are interpreted the same as multiple C-u.
(define-key universal-argument-map [?\M-u] 'universal-argument-more)

;; At some point these apparently became disabled by default.
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Note that M-c is capitalize, and that's somewhat inconsistent with the case
;; change commands prefixed by C-x.
(global-set-key [?\C-x ?l]    'downcase-word)
(global-set-key [?\C-x ?u]    'upcase-word)

(global-set-key [?\C-x ?\M-q] 'toggle-read-only)

(global-set-key [?\C-c ?\;]   'insert-timestamp)
(global-set-key [?\C-c ?']    'insert-fixme)
(global-set-key [?\C-x ?\M-u] 'my-browse-url)
(global-set-key [f8]          'gdb)

;; Dangerously similar to C-x C-c, but I set confirm-kill-emacs.
(global-set-key [?\C-x ?c]    'calc)

(global-set-key [?\M-g ?\M-m] 'my-prefix-menu-modes)
(global-set-key [?\M-g ?\M-v] 'my-prefix-menu-misc)

;; Make apropos show interactive and non-interactive functions.
(setq apropos-do-all t)

;; Separate apropos command to search variables names.
(global-set-key [?\C-h ?A]    'apropos-variable)

(setq Man-width (/ split-width-threshold 2))  ; default is frame width (wrong)
(global-set-key [?\C-h ?M]    'manual-entry)

(global-set-key [?\C-x ?r ?z] 'delete-whitespace-rectangle)

(global-set-key [?\M-+]       'sort-lines)

;; Using M-x as an alias for C-x enables faster typing of C-x sequences that
;; don't involve the Ctrl key.  Moving the traditional M-x to double M-x is an
;; acceptable inconvenience to me.
(global-set-key [?\M-x]       'Control-X-prefix)
(global-set-key [?\C-x ?\M-x] 'execute-extended-command)
