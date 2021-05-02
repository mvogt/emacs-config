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


;;
;; tmux
;;

;; Same as turnip-yank-from-buffer, but update to work with latest tmux. The
;; format variable "line" doesn't work with the command tmux list-buffers, and
;; the command "tmux show-buffer -b foo" requires foo to be the buffer name
;; rather than its index.
(defun my-turnip-yank-from-buffer (&optional buffer)
  "Yank from tmux buffer with index BUFFER.
If no argument is provided the paste-buffer is used."
  (interactive
   (let* ((buffers (turnip:call->lines
                    "list-buffers" "-F" "#{buffer_name}: #{buffer_sample}"))
          (choice (completing-read "Buffer: " buffers nil 'confirm)))
     (list (when (string-match "^buffer[0-9]+" choice)
             (match-string-no-properties 0 choice)))
   )
  )
  (insert (apply #'turnip:call "show-buffer"
                 (when buffer (list "-b" buffer))))
)

(require 'my-region-or-line "kill-yank")
(defvar my-turnip-send-hist '())
(defun my-turnip-send-text (&optional region-p)
  "Prompt for text, and send it to a tmux pane.
With a universal prefix, the prompt is pre-filled with the active region.
If no region is active, use the current line.
In either case, trim leading and trailing whitespace."
  (interactive "P")
  (let ((cmd (read-string "Send text: " (if region-p (my-region-or-line) "")
                          'my-turnip-send-hist))
        (target (call-interactively #'turnip-choose-pane)))
    (unless (called-interactively-p 'any)
      (setq target (turnip:normalize-and-check-target-pane target)))
    (turnip:send-keys target cmd)
  )
)

(defvar my-turnip-hostname-hist '())
(defun my-turnip-connect (cmd &optional region-p)
  "Open new tmux window with specified connection command. Prompt for hostname
to connect to. Use the hostname as the name of the new window.
With a universal prefix, pre-fill the hostname prompt with the active region
or current line."
  (interactive "P")
  (let ((host (read-string (format "%s: " cmd)
                           (if region-p (my-region-or-line) "")
                           'my-turnip-hostname-hist)))
    (turnip:call "new-window" "-n" host (format "%s %s" cmd host))
  )
)

;; https://github.com/abo-abo/hydra
(defhydra my-tmux-menu (:color blue)
  "
tmux:
_y_ Yank from tmux buffer         _S_ SSH in new tmux window
_t_ Send text to tmux pane        _R_ SSH with Ansible key in new tmux window
_r_ Send region to tmux pane      _T_ Telnet in new tmux window
_b_ Send region to tmux buffer

_p_ Select tmux pane for future commands
_c_ tmux command builder (C-RET to finalize)
"
  ("R" (my-turnip-connect "tmux-ssh-ansible" current-prefix-arg) nil)
  ("S" (my-turnip-connect "tmux-ssh" current-prefix-arg) nil)
  ("T" (my-turnip-connect "tmux-telnet" current-prefix-arg) nil)

  ;; FIXME-mvogt-20210502: The prompt for destination buffer is unintuitive.
  ;; It's asking for a name to create, and it gives a useless list of index
  ;; numbers for existing buffers. Replace this function with my own that
  ;; prompts in a different way.
  ("b" turnip-send-region-to-buffer nil)

  ("c" turnip-command nil)
  ("p" turnip-choose-pane nil)
  ("r" turnip-send-region nil)
  ("t" my-turnip-send-text nil)
  ("y" my-turnip-yank-from-buffer nil)
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
(global-set-key [?\M-g ?\M-t] 'my-tmux-menu/body)

(global-set-key [remap fill-paragraph] #'fill-paragraph-or-unfill)

;; Make apropos show interactive and non-interactive functions.
(setq apropos-do-all t)

;; Separate apropos command to search variables names.
(global-set-key [?\C-h ?A]    'apropos-variable)

(setq Man-width (/ split-width-threshold 2))  ; default is frame width (wrong)
(global-set-key [?\C-h ?M]    'manual-entry)

(global-set-key [?\C-x ?r ?z] 'delete-whitespace-rectangle)

(global-set-key [?\M-+]       'sort-lines)

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; See also mac-option-modifier and mac-control-modifier.
)
