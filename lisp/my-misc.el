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
  (let ((url-suffix (replace-regexp-in-string
                     "\\s +" "+"
                     (replace-regexp-in-string "+" "%2B" terms))))
    (list (concat "https://duckduckgo.com/?q=" url-suffix))
  )
)

(defvar my-jira-base-url "https://jira.atlassian.com/browse/")
(defvar my-jira-default-prefix "JRA-")

(autoload 'browse-url "browse-url")
(require 'my-cur-word-or-region "grep-compile")
(defun my-browse-url ()
  "Wrapper for browse-url.  Prompts for URL, search, or Jira issue.
Defaults to the word around the point or the active region as the default.
Search terms are passed as a single string to func my-format-search-url,
which should return a URL string."
  (interactive)
  (message
   "[U]RL, [S]earch, [D]ictionary, [J]ira, [C]ommander, [P]ython, or [T]icker?")
  (let ((which-func (read-char))
        (context (my-cur-word-or-region)))
    (cond
     ((= which-func ?u)
      (call-interactively 'browse-url))
     ((= which-func ?s)
      (dolist (url (my-format-search-url
                    (read-string "Search terms: " context)))
        ;; I set the "generic" browser to Chrome in my ~/.emacs.local.el file.
        ;; Using it here works around an unexplained delay with Firefox when
        ;; calling browse-url twice in a row.
        (if browse-url-generic-program
            (browse-url-generic url)
          (browse-url url))
      ))
     ((= which-func ?d)
      (browse-url (format "http://www.onelook.com/?w=%s"
                          (read-string "OneLook.com dictionary search: "
                                       context))))
     ((= which-func ?j)
      (browse-url
       (concat my-jira-base-url
               (read-string "Jira issue: "
                            (if (string-match "^[A-Za-z]*-[0-9]*$" context)
                                context
                              (concat my-jira-default-prefix context))))))
     ((= which-func ?c)
      (browse-url (format "%s%s" my-ecommander-base-url
                          (read-string "Electric Commander job ID: "
                                       context))))
     ((= which-func ?t)
      (browse-url (format "http://finance.yahoo.com/q/pr?s=%s+Profile"
                          (read-string "Stock ticker symbol: " context))))
     ((= which-func ?p)
      (browse-url (format "http://docs.python.org/2.7/library/%s.html"
                          (read-string "Python 2.7 library: " context))))
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

;; Written by Artur Malabarba
;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun fill-paragraph-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column (if (eq last-command 'fill-paragraph-or-unfill)
                         (progn
                           (setq this-command nil)
                           (point-max)
                         )
                       fill-column)))
    (call-interactively #'fill-paragraph)
  )
)

;; insert-pair wrappers for obvious chars.  Just like insert-parentheses.
;; If I was smarter, I would use a macro.
(defun my-enclose-double-quotes (&optional qty)
  (interactive "P")
  (insert-pair qty ?\" ?\")
)
(defun my-enclose-single-quotes (&optional qty)
  (interactive "P")
  (insert-pair qty ?' ?')
)
(defun my-enclose-back-quotes (&optional qty)
  (interactive "P")
  (insert-pair qty ?` ?`)
)
(defun my-enclose-braces (&optional qty)
  (interactive "P")
  (insert-pair qty ?{ ?})
)
(defun my-enclose-square-brackets (&optional qty)
  (interactive "P")
  (insert-pair qty ?[ ?])
)
(defun my-enclose-angle-brackets (&optional qty)
  (interactive "P")
  (insert-pair qty ?< ?>)
)
(defun my-enclose-tag (&optional qty)
  (interactive "P")
  (let* ((tag-open (read-string "Tag (with optional args): "))
         (tag-name (car (split-string tag-open))))
    (insert-pair qty (format "<%s>" tag-open) (format "</%s>" tag-name))
  )
)
;; Undo for the above.
(defun my-enclose-undo ()
  "Delete the characters before and after the active region."
  (interactive)
  (when (and transient-mark-mode mark-active)
    (save-excursion (goto-char (region-end))       (delete-char 1))
    (save-excursion (goto-char (region-beginning)) (delete-char -1))
  )
)

(defun my-get-cur-line-val ()
  "Return everything on the current line after colon space.
If no colon space is found, return nil."
  (let (line-start whole-line val)
    (save-excursion
      (move-beginning-of-line nil)
      (setq line-start (point))
      (move-end-of-line nil)
      (setq whole-line (buffer-substring line-start (point)))
      (if (string-match ": " whole-line)
          (setq val (substring whole-line (match-end 0))))
    )
    val
  )
)

(defun my-copy-cur-line-val ()
  "Interactive wrapper for my-get-cur-line-val that copies to the clipboard."
  (interactive)
  (let ((val (my-get-cur-line-val)))
    (if val (kill-new val))
  )
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
  (message "Mode [d]iff, [t]ext, [h]tml, [x]ml, [s]hell, [c]++, [m]ake, c[p]erl, p[y]thon,
[r]uby, [j]avascript, [J]ava, proto[b]uf, [e]macs lisp, lisp [i]nteraction,
[w]ord wrap, [l]ine numbers, whitesp[a]ce, [k] toggle indent w/tabs?")
  (let ((which-func (read-char)))
    (cond
     ((= which-func ?d) (diff-mode))
     ((= which-func ?t) (text-mode))
     ((= which-func ?h) (html-mode))
     ((= which-func ?x) (xml-mode))
     ((= which-func ?s) (sh-mode))
     ((= which-func ?c) (c++-mode))
     ((= which-func ?m) (makefile-mode))
     ((= which-func ?p) (cperl-mode))
     ((= which-func ?y) (python-mode))
     ((= which-func ?r) (ruby-mode))
     ((= which-func ?j) (js-mode))
     ((= which-func ?J) (java-mode))
     ((= which-func ?b) (protobuf-mode))
     ((= which-func ?e) (emacs-lisp-mode))
     ((= which-func ?i) (lisp-interaction-mode))
     ((= which-func ?w) (visual-line-mode))
     ((= which-func ?l) (linum-mode))
     ((= which-func ?a) (call-interactively 'whitespace-mode))
     ((= which-func ?k) (toggle-indent-tabs-mode))
    )
  )
)

(defun my-prefix-menu-misc ()
  "Display a shortcut menu in the minibuffer of miscellaneous
functions that I don't use often enough to bind to keys of their
own."
  (interactive)
  (message "Call [e]val buf, eval [r]egion, cmd [h]ist, browse [k]ill ring,
[m]an cleanup, [x] unfontify, [t]abify, [u]ntabify,
describe [c]har, describe ke[y], [g]db many windows, i[s]earch fwd word,
re[n]ame uniquely, [l]ist colors, read col[o]r, customize [f]ace?")
  (let ((which-func (read-char)))
    (cond
     ((= which-func ?e) (eval-buffer))
     ((= which-func ?r) (eval-region (point) (mark)))
     ((= which-func ?h) (describe-variable 'command-history))
     ((= which-func ?k) (browse-kill-ring))
     ((= which-func ?m) (my-man-cleanup))
     ((= which-func ?x) (my-unfontify))
     ((= which-func ?t) (call-interactively 'tabify))
     ((= which-func ?u) (call-interactively 'untabify))
     ((= which-func ?c) (call-interactively 'describe-char))
     ((= which-func ?y) (my-describe-key))
     ;; Invoke after M-x gdb. Provided by built-in gdb-ui.el.
     ((= which-func ?g) (call-interactively 'gdb-many-windows))
     ((= which-func ?s) (isearch-forward-word))
     ((= which-func ?n) (rename-uniquely))
     ((= which-func ?l) (list-colors-display))
     ((= which-func ?o) (call-interactively 'read-color))
     ((= which-func ?f) (call-interactively 'customize-face))
    )
  )
)


;;
;; http://orgmode.org
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


(add-hook 'diff-mode-hook
  (function (lambda ()
              (whitespace-mode 1)
              (local-unset-key [?\M-o])
              (local-unset-key [?\M-g]))
  )
)

(defun my-toggle-read-only ()
  (interactive)
  (call-interactively (if (>= emacs-major-version 24)
			  'read-only-mode
			'toggle-read-only))
)

;; Even though it's correct in a fixed-width font to use two spaces after a
;; period at the end of a sentence, and even though I always edit in Emacs in
;; a fixed-width font, it causes no end of compatibility hassles with the rest
;; of the world.
(setq sentence-end-double-space nil)

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

;; I hate the inconsistency, but I'm leaving insert-parentheses on
;; "M-open-paren" because "C-x open-paren" has the venerable
;; kmacro-start-macro.
(global-set-key [?\C-x ?\"]   'my-enclose-double-quotes)
(global-set-key [?\C-x ?']    'my-enclose-single-quotes)   ; was expand-abbrev
(global-set-key [?\C-x ?`]    'my-enclose-back-quotes)     ; was next-error
(global-set-key [?\C-x ?{]    'my-enclose-braces)
(global-set-key [?\C-x ?[]    'my-enclose-square-brackets) ; was backward-page
(global-set-key [?\C-x ?<]    'my-enclose-angle-brackets)  ; was scroll-left
(global-set-key [?\C-x ?>]    'my-enclose-tag)             ; was scroll-right
(global-set-key [?\C-x ?\]]   'my-enclose-undo)            ; was forward-page

(global-set-key [?\C-x ?\M-q] 'my-toggle-read-only)
(global-set-key [?\M-!]       'my-toggle-read-only)

(global-set-key [?\C-c ?\;]   'insert-timestamp)
(global-set-key [?\C-c ?']    'insert-fixme)
(global-set-key [?\C-x ?\M-u] 'my-browse-url)
(global-set-key [f8]          'gdb)

;; Dangerously similar to C-x C-c, but I set confirm-kill-emacs.
(global-set-key [?\C-x ?c]    'calc)

(global-set-key [?\M-g ?\M-m] 'my-prefix-menu-modes)
(global-set-key [?\M-g ?\M-v] 'my-prefix-menu-misc)
(global-set-key [?\M-g ?4]    'my-recursive-grep)
(global-set-key [?\M-g ?\M-4] 'my-recursive-grep)
(global-set-key [?\M-g ?5]    'my-dired-sandbox)
(global-set-key [?\M-g ?\M-5] 'my-dired-sandbox)
(global-set-key [?\M-g ?8]    'gdb)
(global-set-key [?\M-g ?\M-8] 'gdb)
(global-set-key [?\M-g ?9]    'compile)
(global-set-key [?\M-g ?\M-9] 'compile)

(global-set-key [remap fill-paragraph] #'fill-paragraph-or-unfill)

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

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; See also mac-option-modifier and mac-control-modifier.
)
