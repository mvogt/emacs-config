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

;; This saves a tab keystroke, but I find it annoying.
(setq electric-indent-mode nil)

;;----------------------------------------------------------------------------
;; C mode
;;
;; My custom style inherits from the GNU style because it's the closest to
;; what I want.  I call it hybrid because it's a little like the K&R style and
;; uses some stuff from the cc-mode style.
(c-add-style "hybrid"
             '("gnu"
               (c-basic-offset . 4)
               (c-offsets-alist . ((substatement-open . 0)
                                   (statement-case-open . 0)
                                   (brace-list-open . 0)
                                   (case-label . +)
                                   (arglist-intro . +)
                                   (arglist-close . +)
                                   (knr-argdecl-intro . +))))
)

;; Make my hybrid style the default.  Note that I'm replacing the full alist
;; that is the default.  That means I'll need to check the default in each new
;; version of Emacs to see if it changed.  If it changes, I'll need to update
;; my setting here to match.
(setq c-default-style '((java-mode . "java")
                        (other . "hybrid"))
)

;; Make it easier to change the nominal indentation for a given buffer.
(defun set-c-basic-offset (num-spaces)
  "Set the variable c-basic-offset interactively."
  (interactive "NSet c-basic-offset to: ")
  (or (null num-spaces)
      (set-variable (make-local-variable 'c-basic-offset) num-spaces))
)

(require 'set-tab-width "my-indent")
(add-hook 'c-mode-common-hook
  (function (lambda ()
              (hide-ifdef-mode 1)
              (toggle-truncate-lines 1)  ; disable line wrap
              (local-set-key [?\C-c ?i] 'set-c-basic-offset)
              (local-set-key [?\C-c ?-] 'hide-ifdef-block)
              (local-set-key [?\C-c ?=] 'show-ifdef-block)
              (local-set-key [?\C-c ?+] 'show-ifdef-block)
              (local-set-key [?\C-c ?*] 'show-ifdefs)
              (set-tab-width 4))  ; too many coworkers have inferior editors
  )
)

;; Use C++ mode instead of C mode for pretty much any C file.
(add-to-list 'auto-mode-alist
             '("\\.\\([cChHiI]\\|hh\\|cc\\|ii\\|cpp\\|ipp\\|gcov\\)\\'"
               . c++-mode)
)


;;----------------------------------------------------------------------------
;; CPerl mode
;;
;; Note that the color coding in perl-mode is not reliable because Perl syntax
;; is so messy, but in CPerl mode it's better.
;; Note that doing (setq cperl-hairy t) turns on more verbose options.
;; Also note the pull-down menu is an easy place to find more docs.

;; Replace Perl mode with CPerl mode.
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; CPerl mode tries to model itself after C mode, but it's not nearly as
;; elaborate or customizable.  Rather than try to use its crappy version of C
;; mode's style feature, I just blast out the tab settings I want with this
;; func.
(defun cperl-set-indent (num-spaces)
  "Set the various CPerl mode variables for the indent level.
Causes an intuitive indent level matching the specific argument."
  (setq cperl-indent-level num-spaces)
  (setq cperl-continued-statement-offset num-spaces)
  (setq cperl-continued-brace-offset (- num-spaces))
  (setq cperl-label-offset (- num-spaces))
)

(defun cperl-set-indent-interactive (num-spaces)
  "Set the CPerl indent level interactively."
  (interactive "NSet CPerl indent to: ")
  (if (not (null num-spaces))
      (cperl-set-indent num-spaces)
  )
)

(add-hook 'cperl-mode-hook
  (function (lambda ()
              (cperl-set-indent 4)
              (local-set-key [?\C-c ?i] 'cperl-set-indent-interactive)
            )
  )
)


;;----------------------------------------------------------------------------
;; ChangeLog mode
;;
;; Set variables needed for the ChangeLog stuff below.  Also theoretically
;; useful for other stuff, too.
(setq mydomain "mydomain.com")

;; Make function and key mapping to insert a proper time-stamp in a ChangeLog
;; (when in change-log-mode).
(defun changelog-timestamp-id ()
  "Insert a comment with time-stamp in ChangeLog format"
  (interactive)
  (beginning-of-line)
  (insert
    (format "%s  %s  <%s@%s>\n\n\t* \n\n"
            (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
            user-full-name user-login-name mydomain)
    )
)

(add-hook 'change-log-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ?t] 'changelog-timestamp-id))
  )
)


;;----------------------------------------------------------------------------
;; Makefile mode
;;
;; Restore global bindings for these keys with broken, useless mode-specific
;; defaults.
(add-hook 'makefile-mode-hook
  (function (lambda ()
              (local-unset-key [?\M-p])
              (local-unset-key [?\M-n]))
  )
)

(add-to-list 'auto-mode-alist '("/[Mm]akefile\\." . makefile-gmake-mode))


;;----------------------------------------------------------------------------
;; nXML mode
;;
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  ;; From http://www.emacswiki.org/emacs/XmlMode
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        ;; The first and clause avoids running the loop at all if the point is
        ;; at the beginning of the buffer.
        (while (and (< (point-min) (point))
                    (condition-case nil
                        ;; Run this until it throws an error
                        (progn
                          (nxml-backward-up-element)
                          t ; because the nxml func always returns nil
                        )
                      (error nil) ; handle error: return nil to abort loop
                    )
               )
          (setq path (cons (xmltok-start-tag-local-name) path))
        )
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/"))
        )
      )
    )
  )
)

(add-hook 'nxml-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ??] 'nxml-where)
            )
  )
)


;;----------------------------------------------------------------------------
;; YASnippet
;; https://www.emacswiki.org/emacs/Yasnippet
;; https://github.com/joaotavora/yasnippet
;; I use the one from MELPA.
;; Note: To reload snippets, M-x yas-reload-all
;;
(require 'yasnippet)
(setq yas-wrap-around-region t)
(global-set-key [?\C-x ?\C-n] 'yas-insert-snippet)
(global-set-key [?\C-x ?\M-n] 'yas-insert-snippet)

;; Remove built-in snippets from search path because there are too many. Use
;; only mine.
(setq yas-snippet-dirs '(yas--default-user-snippets-dir))

(setq yas-prompt-functions '(yas-x-prompt
                             yas-dropdown-prompt
                             yas-completing-prompt))

(yas-global-mode 1)


;; JavaScript mode is the best built-in mode for JSON and Android.bp files.
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("Android\\.bp\\'" . js-mode))


;; The built-in Sieve mode is broken for the flavor of the language used by my
;; web mail service.
(add-to-list 'auto-mode-alist '("\\.sieve\\'" . c-mode))
