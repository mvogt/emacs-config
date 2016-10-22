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
;; Customizations to HTML mode
;;

;; "Skeletons" (abbreviations)
(define-skeleton my-html-headline-1
  "HTML level 1 headline tags."
  nil "<h1>" _ "</h1>"
)
(define-skeleton my-html-headline-2
  "HTML level 2 headline tags."
  nil "<h2>" _ "</h2>"
)
(define-skeleton my-html-headline-3
  "HTML level 3 headline tags."
  nil "<h3>" _ "</h3>"
)
(define-skeleton my-html-headline-4
  "HTML level 4 headline tags."
  nil "<h4>" _ "</h4>"
)
(define-skeleton my-html-headline-5
  "HTML level 5 headline tags."
  nil "<h5>" _ "</h5>"
)
(define-skeleton my-html-unordered-list
  "HTML unordered list tags."
  nil "<ul class=lvl1>" \n "<li>" _ \n "</ul>"
)
(define-skeleton my-html-ordered-list
  "HTML ordered list tags."
  nil "<ol class=lvl1>" \n "<li>" _ \n "</ol>"
)
(define-skeleton my-html-list-item
  "HTML list item tag."
  nil "<li>"
)
(define-skeleton my-html-table
  "HTML table tag."
  nil "<table border=1 cellspacing=0 cellpadding=2>" \n
  "<tr>" _ \n "</table>"
)
(define-skeleton my-html-table-row
  "HTML table row tag."
  nil "<tr>"
)
(define-skeleton my-html-table-column
  "HTML table column tag."
  nil "<td>"
)
(define-skeleton my-html-anchor-simple
  "HTML anchor tag."
  "URL: "
  '(setq input "http://www..com/")
  "<a href=\"" str "\">" _ "</a>"
)
(define-skeleton my-html-anchor-standard
  "HTML anchor tag with URL also in visible text."
  "URL: "
  '(setq input "http://www..com/")
  "<a href=\"" str "\">" str "</a>"
)
(define-skeleton my-html-paragraph
  "HTML paragraph tag."
  nil "<p>"
)
(define-skeleton my-html-paragraph-css
  "HTML paragraph tag with class property."
  "Class: "
  "<p class=" str ">"
)
(define-skeleton my-html-emphasis
  "HTML emphasis tags."
  nil "<em>" _ "</em>"
)
(define-skeleton my-html-strong
  "HTML strong (i.e. bold) tags."
  nil "<strong>" _ "</strong>"
)
(define-skeleton my-html-blockquote
  "HTML blockquote tags."
  nil "<blockquote>" \n _ \n "</blockquote>"
)
(define-skeleton my-html-code
  "HTML (source) code tags."
  nil "<code>" _ "</code>"
)
(define-skeleton my-html-kbd
  "HTML keyboard tags."
  nil "<kbd>" _ "</kbd>"
)
(define-skeleton my-html-sample
  "HTML sample text tags."
  nil "<samp>" _ "</samp>"
)
(define-skeleton my-html-teletype
  "HTML teletype text tags."
  nil "<tt>" _ "</tt>"
)

;; Define local key bindings and mouse menu for HTML mode.  This gets
;; used in the mode hook.
(defvar my-html-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "HTML")))
    ; Can't remember why I formerly preferred this setting.
    ;(define-key map [?\t] 'indent-relative-maybe)
    (define-key map [?\C-c ?\C-n] 'html-name-char)
    (if (< emacs-major-version 21)
        (define-key map [?\M-q] 'my-html-fill-para)
    )
    (if (< emacs-major-version 21)
        (define-key map [?\C-c ?\C-q] 'fill-paragraph)
    )
    (define-key map [?\C-c ?1] 'my-html-headline-1)
    (define-key map [?\C-c ?2] 'my-html-headline-2)
    (define-key map [?\C-c ?3] 'my-html-headline-3)
    (define-key map [?\C-c ?4] 'my-html-headline-4)
    (define-key map [?\C-c ?5] 'my-html-headline-5)
    (define-key map [?\C-c ?u] 'my-html-unordered-list)
    (define-key map [?\C-c ?o] 'my-html-ordered-list)
    (define-key map [?\C-c ?l] 'my-html-list-item)
    (define-key map [?\C-c ?t] 'my-html-table)
    (define-key map [?\C-c ?r] 'my-html-table-row)
    (define-key map [?\C-c ?c] 'my-html-table-column)
    (define-key map [?\C-c ?a] 'my-html-anchor-simple)
    (define-key map [?\C-c ?\C-a] 'my-html-anchor-standard)
    (define-key map [?\C-c ?p] 'my-html-paragraph)
    (define-key map [?\C-c ?\C-p] 'my-html-paragraph-css)
    (define-key map [?\C-c ?e] 'my-html-emphasis)
    (define-key map [?\C-c ?s] 'my-html-strong)
    (define-key map [?\C-c ?b] 'my-html-blockquote)
    (define-key map [?\C-c ?d] 'my-html-code)
    (define-key map [?\C-c ?k] 'my-html-kbd)
    (define-key map [?\C-c ?g] 'my-html-sample)
    (define-key map [?\C-c ?y] 'my-html-teletype)
    (define-key map [menu-bar html] (cons "HTML" menu-map))
    (define-key menu-map [amp-code] '("Ampersand code" . html-name-char))
    (define-key menu-map [samp] '("Sample" . my-html-sample))
    (define-key menu-map [kbd] '("Keyboard" . my-html-kbd))
    (define-key menu-map [code] '("Code" . my-html-code))
    (define-key menu-map [blkq] '("Blockquote" . my-html-blockquote))
    (define-key menu-map [strong] '("Strong" . my-html-strong))
    (define-key menu-map [em] '("Emphasis" . my-html-emphasis))
    (define-key menu-map [dash4] '("--"))    ; dividing line
    (define-key menu-map [p2] '("Paragraph CSS" . my-html-paragraph-css))
    (define-key menu-map [p1] '("Paragraph" . my-html-paragraph))
    (define-key menu-map [a2] '("Anchor standard" . my-html-anchor-standard))
    (define-key menu-map [a1] '("Anchor simple" . my-html-anchor-simple))
    (define-key menu-map [dash3] '("--"))    ; dividing line
    (define-key menu-map [rd] '("Column" . my-html-table-column))
    (define-key menu-map [tr] '("Row" . my-html-table-row))
    (define-key menu-map [table] '("Table" . my-html-table))
    (define-key menu-map [dash2] '("--"))    ; dividing line
    (define-key menu-map [li] '("List item" . my-html-list-item))
    (define-key menu-map [ol] '("Ordered list" . my-html-ordered-list))
    (define-key menu-map [ul] '("Unordered list" . my-html-unordered-list))
    (define-key menu-map [dash1] '("--"))    ; dividing line
    (define-key menu-map [h5] '("Headline 5" . my-html-headline-5))
    (define-key menu-map [h4] '("Headline 4" . my-html-headline-4))
    (define-key menu-map [h3] '("Headline 3" . my-html-headline-3))
    (define-key menu-map [h2] '("Headline 2" . my-html-headline-2))
    (define-key menu-map [h1] '("Headline 1" . my-html-headline-1))
    map
  )
  "Keymap for commands for use in HTML mode."
)

;; Variable and function for filling in ampersand codes.
(defun html-name-char (&optional char)
  "Insert a symbolic character name according to 'html-char-names'.
8 bit chars may be inserted with the meta key as in M-SPC for no break space,
or M-- for a soft hyphen."
  (interactive "*")
  (insert ?&)
  (or char
      (setq char (read-quoted-char "Enter char or octal number"))
  )
  (delete-backward-char 1)
  (insert char)
  (undo-boundary)
  (delete-backward-char 1)
  (insert ?&
	  (or (aref html-char-names char)
	      (format "#%d" char))
	  ?\;
  )
)
(defvar html-char-names
  [nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "nbsp" nil "quot" nil nil nil "amp" nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil "lt" nil "gt" nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "nbsp" "iexcl" "cent" "pound" "curren" "yen" "brvbar" "sect"
   "uml" "copy" "ordf" "laquo" "not" "shy" "reg" "macr"
   "ring" "plusmn" "sup2" "sup3" "acute" "micro" "para" "middot"
   "cedil" "sup1" "ordm" "raquo" "frac14" "half" "frac34" "iquest"
   "Agrave" "Aacute" "Acirc" "Atilde" "Auml" "Aring" "AElig" "Ccedil"
   "Egrave" "Eacute" "Ecirc" "Euml" "Igrave" "Iacute" "Icirc" "Iuml"
   "ETH" "Ntilde" "Ograve" "Oacute" "Ocirc" "Otilde" "Ouml" nil
   "Oslash" "Ugrave" "Uacute" "Ucirc" "Uuml" "Yacute" "THORN" "szlig"
   "agrave" "aacute" "acirc" "atilde" "auml" "aring" "aelig" "ccedil"
   "egrave" "eacute" "ecirc" "euml" "igrave" "iacute" "icirc" "iuml"
   "eth" "ntilde" "ograve" "oacute" "ocirc" "otilde" "ouml" "divide"
   "oslash" "ugrave" "uacute" "ucirc" "uuml" "yacute" "thorn" "yuml"
  ]
  "Vector of symbolic character names without `&' and `;'."
)

;; Ugly work-around for filling <li> paragraphs in HTML mode to look
;; exactly like I want them.  The key map above puts ordinary
;; fill-paragraph on C-c C-q just in case.
(defun my-html-fill-para ()
  "Insert newlines before and after the current paragraph, fill the
paragraph, and remove the newlines.  A paragraph is defined as starting
with <li>."
  (interactive)
  (let ((orig-point (point))
        (line-blank (is-line-blank)))
    (if line-blank (search-forward-regexp "<[(li)(p)]" nil t))
    (my-html-fp-middle)
    (if line-blank (goto-char orig-point))
  )
)

(defun is-line-blank ()
  "Return t if the current line contains only white space.  Otherwise
return nil."
  (save-excursion
    (beginning-of-line)
    (posix-looking-at "\\s *$")
  )
)

(defun my-html-fp-middle ()
  (let ((orig-point (point))
        para-start)
    (end-of-line)
    (search-backward-regexp "<[(li)(p)]" nil t)
    (beginning-of-line)
    (newline)
    (setq para-start (point))
    (end-of-line)
    (search-forward-regexp "<[(li)(p)]" nil t)
    (beginning-of-line)
    (newline)
    (goto-char (+ orig-point 1))
    (fill-paragraph nil)
    (setq orig-point (point))
    (goto-char para-start)
    (backward-delete-char 1)
    (end-of-line)
    (search-forward-regexp "<[(li)(p)]" nil t)
    (beginning-of-line)
    (backward-delete-char 1)
    (goto-char (- orig-point 1))
  )
)

;; Finally, define the mode hook for all the HTML crap above
(add-hook 'html-mode-hook
  (function (lambda ()
    (use-local-map my-html-mode-map)
    (if (>= emacs-major-version 21)
        ; These are just the SGML defaults with a typo fixed.
        (let ((paragraph-delimiter
"[ \t]*$\\|[ \t]*</?\\([A-Za-z]\\([-_.:[:alnum:]= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>"
             ))
          (setq paragraph-start paragraph-delimiter)
          (setq paragraph-separate (concat paragraph-delimiter "$"))
        )
    )
  ))
)

(add-hook 'css-mode-hook
  (function (lambda ()
    (rainbow-mode)
  ))
)

;; Affects HTML mode and anything else derived from SGML mode.
(require 'set-tab-width "my-indent")
(add-hook 'sgml-mode-hook
  (function (lambda ()
    (set-tab-width 4)   ; too many coworkers have inferior editors
    (local-set-key [?\C-c ?\C-i] 'set-tab-width)
  ))
)
