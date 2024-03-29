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
(defvar my-tag-wrap-hist '())
(defun my-enclose-tag (&optional qty)
  (interactive "P")
  (let* ((tag-open (read-string "Tag (with optional args): "
                                nil 'my-tag-wrap-hist))
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
  ("l" (call-interactively (if (boundp 'display-line-numbers)
                               'display-line-numbers-mode
                             'linum-mode))
       nil)
  ("m" makefile-mode nil)
  ("p" cperl-mode nil)
  ("r" ruby-mode nil)
  ("s" sh-mode nil)
  ("t" text-mode nil)
  ("v" visual-line-mode nil)
  ("x" xml-mode nil)
  ("y" python-mode nil)
)

(defvar my-vlc-url-hist '())

;; https://github.com/abo-abo/hydra
(defhydra my-misc-menu (:color blue)
  "
Call:
_e_ eval-buffer    _f_ customize-face            _v_ Open in VLC
_t_ tabify         _c_ describe-char             _s_ isearch-forward-word
_u_ untabify       _y_ single-key-description    _n_ rename-uniquely
_x_ Unfontify      _h_ Show command-history      _m_ Manual page cleanup
^ ^                _j_ Directory tree (C-u to prompt for start dir)
"
  ("c" describe-char nil)
  ("e" eval-buffer nil)
  ("f" customize-face nil)
  ("h" (describe-variable 'command-history) nil)
  ("j" my-dirtree-wrapper nil)
  ("m" my-man-cleanup nil)
  ("n" rename-uniquely nil)
  ("s" isearch-forward-word nil)
  ("t" tabify nil)
  ("u" untabify nil)
  ("v" (call-process "vlc" nil 0 nil
                     (read-string "VLC URL: " (my-cur-word-or-region)
                                  'my-vlc-url-hist))
       nil)
  ("x" my-unfontify nil)
  ("y" my-describe-key nil)
)


;; Avy
;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything/
;;
;; We import the whole file with require even though it has proper autoloads
;; and even though all the symbols we reference should be defined. The reason
;; is some strange behavior at Emacs startup. Modifying avy-dispatch-alist
;; causes my-frame-create-hook not to execute.
(require 'avy)
(global-set-key [?\M-\=] 'avy-goto-char-timer)
(define-key isearch-mode-map [?\M-\=] 'avy-isearch)
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt)
)
;; On Avy's action menu, map SPC to the custom function above.
(add-to-list 'avy-dispatch-alist '(?  . avy-action-mark-to-char))

;; https://github.com/abo-abo/hydra
(defhydra my-avy-menu (:color blue)
  "
Avy goto:
_c_ char-timer    _s_ subword-1    _l_ line
_w_ word-1        _y_ symbol-1     _w_ whitespace-end-1
"
  ("c" avy-goto-char-timer nil)
  ("l" avy-goto-line nil)
  ("s" avy-goto-subword-1 nil)
  ("w" avy-goto-whitespace-end nil)
  ("w" avy-goto-word-1 nil)
  ("y" avy-goto-symbol-1 nil)
)


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

(global-set-key [?\M-g ?\M-m] 'my-mode-menu/body)
(global-set-key [?\M-g ?\M-v] 'my-misc-menu/body)
(global-set-key [?\M-g ?\M-f] 'my-avy-menu/body)

(global-set-key [remap fill-paragraph] #'fill-paragraph-or-unfill)

;; Make apropos show interactive and non-interactive functions.
(setq apropos-do-all t)

;; In the output of describe-bindings (F1 b), group by mode.
(setq describe-bindings-outline t)

;; When querying help for an autoload function that hasn't loaded yet, this
;; loads it before retrieving the help text.
(setq help-enable-symbol-autoload t)

;; More and better info in help buffers. From package helpful:
;; https://github.com/Wilfred/helpful
(global-set-key [?\C-h ?f]    'helpful-callable)
(global-set-key [?\C-h ?v]    'helpful-variable)
(global-set-key [?\C-h ?k]    'helpful-key)

;; Separate apropos command to search variables names.
(global-set-key [?\C-h ?A]    'apropos-variable)

(setq Man-width (/ split-width-threshold 2))  ; default is frame width (wrong)
(global-set-key [?\C-h ?M]    'manual-entry)

;; Show a nice, readable dump of a keymap symbol name. This is much easier
;; than finding it in the source code.
(global-set-key [?\C-h ?y]    'describe-keymap)

(global-set-key [?\C-x ?r ?z] 'delete-whitespace-rectangle)

(global-set-key [?\M-+]       'sort-lines)

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; See also mac-option-modifier and mac-control-modifier.
)
