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
(defun my-del-from-alist-by-cdr (lst victim)
  "Delete an element from alist 'lst'
by looking for the cdr of that matches 'victim'."
  (delete-if (lambda (el) (eq (cdr el) victim))
             (symbol-value lst))
)

;; Helm
;;
(require 'helm)
(require 'helm-config)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-ff-file-name-history-use-recentf t)

(global-set-key [?\M-g ?b]    'helm-multi-files)
(global-set-key [?\M-g ?\M-b] 'helm-multi-files)
(global-set-key [?\M-i]       'helm-find-files)
(global-set-key [?\C-x ?\C-f] 'helm-find-files)
(global-set-key [?\C-x ?\M-f] 'helm-find-files)
(global-set-key [?\M-y]       'helm-show-kill-ring)  ; replaces yank-pop

;; This function is hard to describe. It basically means do the intuitive
;; thing. For some reason, the default for this is C-z.
(define-key helm-map [tab]   'helm-execute-persistent-action)
(define-key helm-map [?\C-i] 'helm-execute-persistent-action)
;; Most helm sessions have a choice of different actions you can perform on
;; your selected files. This toggles to a menu showing those action choices.
(define-key helm-map [backtab] 'helm-select-action)

(helm-mode 1)

;; Prevents helm from activating on any of these actions.
(loop for trigger in '((dired-do-rename)
                       (dired-do-copy)
                       (dired-do-symlink)
                       (dired-do-relsymlink)
                       (dired-do-hardlink)
                       (write-file)
                       (basic-save-buffer))
      do (add-to-list 'helm-completing-read-handlers-alist trigger))

;; Action menu entries I never use. Declutter those menus.
(let ((blacklist '(find-alternate-file
                   find-file-other-frame
                   helm-buffers-browse-project
                   helm-ediff-marked-buffers
                   helm-ff-browse-project
                   helm-ff-cache-add-file
                   helm-ff-etags-select
                   helm-ff-fd
                   helm-ff-gid
                   helm-ff-locate
                   helm-ff-mail-attach-files
                   helm-ff-pdfgrep
                   helm-ff-print
                   helm-ff-query-replace-fnames-on-marked
                   helm-ff-switch-to-shell
                   helm-ff-touch-files
                   helm-files-insert-as-org-link
                   helm-find-file-as-root
                   helm-find-files-backup
                   helm-find-files-ediff-files
                   helm-find-files-ediff-merge-files
                   helm-find-files-eshell-command-on-file
                   helm-find-files-grep
                   helm-find-files-hardlink
                   helm-marked-files-in-dired
                   helm-open-file-externally
                   hexl-find-file
                   switch-to-buffer-other-frame
                   switch-to-buffer-other-tab)))
  (loop for action-menu in '(helm-find-files-actions
                             helm-type-file-actions
                             helm-type-buffer-actions)
        do (mapcar (lambda (victim)
                     (my-del-from-alist-by-cdr action-menu victim))
                   blacklist)
  )
)
;; These are replaced below
(mapcar
 (lambda (victim) (my-del-from-alist-by-cdr 'helm-find-files-actions victim))
 '(helm-ff-find-sh-command helm-find-files-other-window))

;; To search paths below the current location, C-s is much more intuitive
;; than the default "C-c /". The original function on C-s is still
;; available on "M-g s".
(add-to-list
 'helm-find-files-actions
 '("Search subtree paths `C-s'" . helm-ff-find-sh-command) t)
(define-key helm-find-files-map [?\C-s] 'helm-ff-run-find-sh-command)

;; To open the selection in the other window, C-o is much easier to type
;; than the default "C-c o".
(add-to-list
 'helm-find-files-actions
 '("Open file other window `C-o'" . helm-find-files-other-window) t)
(define-key helm-find-files-map    [?\C-o] 'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map [?\C-o] 'helm-ff-run-switch-other-window)
(define-key helm-buffer-map        [?\C-o] 'helm-buffer-switch-other-window)

;; https://github.com/abo-abo/hydra
(defhydra my-helm-main (:color blue)
  "
Helm:
_m_ Local and global mark rings     _o_ Search current buffer with helm-occur
_b_ Bookmarks                       _i_ Imenu current buffer
_r_ Recent files                    _I_ Imenu all buffers
_f_ Menu of buffers, recent files, bookmarks, and current dir
_R_ Resume last Helm (with prefix, first select from multiple)

_w_ Emacs registers                 _M_ Manual pages (using WOman)
_c_ Colors                          _X_ Helm docs
_C_ Complete Emacs Lisp symbol      _Y_ GNU Info
_p_ Emacs packages                  _Z_ GNU Info for Emacs topics only
_P_ Emacs processes
"
  ("C" helm-lisp-completion-at-point nil)
  ("I" helm-imenu-in-all-buffers nil)
  ("M" helm-man-woman nil)
  ("P" helm-list-emacs-process nil)
  ("R" helm-resume nil)
  ("X" helm-documentation nil)
  ("Y" helm-info-at-point nil)
  ("Z" helm-info-emacs nil)
  ("b" helm-filtered-bookmarks nil)
  ("c" helm-colors nil)
  ("f" helm-multi-files nil)
  ("i" helm-imenu nil)
  ("m" helm-all-mark-rings nil)
  ("o" helm-occur nil)
  ("p" helm-list-elisp-packages nil)
  ("r" helm-recentf nil)
  ("w" helm-register nil)
)

(global-set-key [?\C-h ?a]    'helm-apropos)
;; I never use the default prefix (C-x c). Instead, I use a hydra with a
;; subset of the helm functions available through the prefix key.
(global-set-key [?\M-g ?h]    'my-helm-main/body)
(global-set-key [?\M-g ?\M-h] 'my-helm-main/body)


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
  (insert-pair qty ?\[ ?\])
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

;; https://github.com/abo-abo/hydra
(defhydra my-mode-menu (:color blue)
  "
Switch mode:
_d_ Diff       _s_ Bash          _y_ Python
_t_ Text       _c_ C++           _r_ Ruby
_h_ HTML       _m_ Makefile      _j_ Javascript
_x_ XML        _p_ CPerl         _J_ Java
_v_ Visual lines (toggle)    ^^  _b_ Protobuf
_l_ Line numbers (toggle)    ^^  _e_ Emacs Lisp
_a_ Show whitespace (toggle) ^^  _i_ Emacs Lisp interactive
_k_ Indent with tabs (toggle)
"
  ("J" java-mode nil)
  ("a" whitespace-mode nil)
  ("b" protobuf-mode nil)
  ("c" c++-mode nil)
  ("d" diff-mode nil)
  ("e" emacs-lisp-mode nil)
  ("h" html-mode nil)
  ("i" lisp-interaction-mode nil)
  ("j" js-mode nil)
  ("k" toggle-indent-tabs-mode nil)
  ("l" linum-mode nil)
  ("m" makefile-mode nil)
  ("p" cperl-mode nil)
  ("r" ruby-mode nil)
  ("s" sh-mode nil)
  ("t" text-mode nil)
  ("v" visual-line-mode nil)
  ("x" xml-mode nil)
  ("y" python-mode nil)
)

;; https://github.com/abo-abo/hydra
(defhydra my-misc-menu (:color blue)
  "
Call:
_e_ eval-buffer    _f_ customize-face            _v_ Open in VLC
_t_ tabify         _c_ describe-char             _s_ isearch-forward-word
_u_ untabify       _y_ single-key-description    _n_ rename-uniquely
_x_ Unfontify      _h_ Show command-history      _m_ Manual page cleanup
"
  ("c" describe-char nil)
  ("e" eval-buffer nil)
  ("f" customize-face nil)
  ("h" (describe-variable 'command-history) nil)
  ("m" my-man-cleanup nil)
  ("n" rename-uniquely nil)
  ("s" isearch-forward-word nil)
  ("t" tabify nil)
  ("u" untabify nil)
  ("v" (call-process "vlc" nil 0 nil
                     (read-string "VLC URL: " (my-cur-word-or-region)))
       nil)
  ("x" my-unfontify nil)
  ("y" my-describe-key nil)
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

;; Avoid performance issues in files with very long lines.
(if (fboundp 'global-so-long-mode)
    (global-so-long-mode 1))

;; Even though it's correct in a fixed-width font to use two spaces after a
;; period at the end of a sentence, and even though I always edit in Emacs in
;; a fixed-width font, it causes no end of compatibility hassles with the rest
;; of the world.
(setq sentence-end-double-space nil)

;; When navigating to the beginning of the minibuffer, go to the beginning of
;; the user input rather than the actual beginning of the buffer.
(setq minibuffer-beginning-of-buffer-movement t)

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
(global-set-key [?\C-x ?\[]   'my-enclose-square-brackets) ; was backward-page
(global-set-key [?\C-x ?<]    'my-enclose-angle-brackets)  ; was scroll-left
(global-set-key [?\C-x ?>]    'my-enclose-tag)             ; was scroll-right
(global-set-key [?\C-x ?\]]   'my-enclose-undo)            ; was forward-page

(global-set-key [?\C-x ?\M-q] 'my-toggle-read-only)
(global-set-key [?\M-!]       'my-toggle-read-only)

(global-set-key [?\C-c ?\;]   'insert-timestamp)
(global-set-key [?\C-c ?']    'insert-fixme)
(global-set-key [f8]          'gdb)

(global-set-key [?\M-g ?\M-m] 'my-mode-menu/body)
(global-set-key [?\M-g ?\M-v] 'my-misc-menu/body)
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
(global-set-key [?\C-x ?\M-x] 'helm-M-x)

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; See also mac-option-modifier and mac-control-modifier.
)
