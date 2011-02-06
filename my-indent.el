;;----------------------------------------------------------------------------
;; Indentation
;;
;; Don't use hard tabs at all when indenting.
(setq-default indent-tabs-mode nil)

;; See my-set-indent-basic-offset in this file.
(setq-default indent-basic-offset 4)

;; This variable first appeared in Emacs 22.  It's part of the newer SGML/HTML
;; indentation algorithm, and its default is an unsightly 2.
(setq-default sgml-basic-offset 4)

;; The definition of this func that comes with Emacs is in
;; /usr/share/emacs/23.1/lisp/indent.el.gz.
;; It does a whole bunch of stuff that I don't need.  I trip over one of
;; those: if the region is active, it indents the whole region.  So, I've
;; trimmed it down to the only two cases I care about.
(defun indent-for-tab-command (&optional arg)
  "Replacement for complex default func that just does the stuff I want."
  (interactive "P")
  (cond
   ((or ;; indent-to-left-margin is only meant for indenting,
	;; so we force it to always insert a tab here.
	(eq indent-line-function 'indent-to-left-margin)
	(and (not tab-always-indent)
	     (or (> (current-column) (current-indentation))
		 (eq this-command last-command))))
    (insert-tab arg))
   (t
    (funcall indent-line-function))
  )
)

(defun my-set-indent-basic-offset (num-spaces)
  "Set the variable indent-basic-offset interactively."
  (interactive "NSet indent-basic-offset to: ")
  (or (null num-spaces)
      (set-variable (make-local-variable 'indent-basic-offset) num-spaces))
)

(defun set-tab-width (num-spaces)
  "Set the variable tab-width interactively."
  (interactive "NSet tab-width to: ")
  (or (null num-spaces)
      (set-variable (make-local-variable 'tab-width) num-spaces))
)
(provide 'set-tab-width)

(defun basic-indent (tab-stop increment-p)
  "Indent or unindent the current line to a uniform tab stop.
The uniform distance between tabs is specified by tab-stop.
The current line is indented if increment-p is non-nil;
otherwise, it's unindented."
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (let* ((cur-col (current-column))
           (misalign (% cur-col tab-stop)))
      (delete-horizontal-space)
      (indent-to (+ cur-col (if increment-p
                                (- tab-stop misalign)
                              (- (if (= misalign 0) tab-stop misalign)))))
    )
  )
)

(global-set-key [?\M-,]       'decrease-left-margin)     ; this is a hack
(global-set-key [?\M-.]       'indent-rigidly)
(global-set-key [C-backspace] 'delete-indentation)
(global-set-key [?\C-c ?i]    'my-set-indent-basic-offset)
(global-set-key [?\C-c ?\C-i] 'set-tab-width)
(global-set-key [?\C-\\]      (lambda () (interactive)
                                (basic-indent indent-basic-offset nil)))
(global-set-key [?\M-i]       (lambda () (interactive)
                                (basic-indent indent-basic-offset t)))
