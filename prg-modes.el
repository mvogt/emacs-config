;;----------------------------------------------------------------------------
;; C mode
;;
;; Override the nominal indentation amount for all C Mode styles.  This will
;; get reset back to a style's own value if the user manually selects a style.
;; I much prefer 2 (as in my "hybrid" style below), but 4 appears to be more
;; popular.
(setq c-basic-offset 4)

;; My custom style inherits from the GNU style because it's the closest to
;; what I want.  I call it hybrid because it's a little like the K&R style.
(c-add-style "hybrid"
             '("gnu" (c-offsets-alist . ((substatement-open . 0)
                                         (statement-case-open . 0)
                                         (brace-list-open . 0)
                                         (case-label . +))))
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

(add-hook 'c-mode-common-hook
  (function (lambda ()
              (hide-ifdef-mode 1)
              (local-set-key [?\C-c ?i] 'set-c-basic-offset)
              (local-set-key [?\C-c ?-] 'hide-ifdef-block)
              (local-set-key [?\C-c ?=] 'show-ifdef-block)
              (local-set-key [?\C-c ?+] 'show-ifdef-block)
              (local-set-key [?\C-c ?*] 'show-ifdefs)
              (set-tab-width 4)  ; too many coworkers have inferior editors
              ;; So I can store my snippets for all C modes in one dir.
              (setq yas/mode-symbol 'c++-mode))
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
  "Set the CPerl indetn level interactively."
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
;; YASnippet
;; http://code.google.com/p/yasnippet/
;;
(let ((my-yas-path "~/src/emacs/yasnippet-0.6.1c")
      (my-snippets-path (concat my-startup-file-path "snippets")))
  (if (and (file-accessible-directory-p my-yas-path)
           (file-accessible-directory-p my-snippets-path))
      (progn
        (add-to-list 'load-path my-yas-path t)
        (require 'yasnippet)
        (yas/initialize)
        ;; Reload with yas/reload-all
        (yas/load-directory my-snippets-path)

        (setq yas/wrap-around-region t)
        (global-set-key [?\C-x ?\C-n] 'yas/insert-snippet)
      )
  )
)
