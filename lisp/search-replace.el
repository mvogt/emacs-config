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
;; Search and replace
;;
(defun replace-special-chars ()
  "Replace characters that are not printable in 7-bit ASCII with something
that is.  These obnoxious special chars typically come from MS Word or
Outlook."
  (interactive)
  (let ((search-for)
        (replace-with)
        (special-chars (list (cons (char-to-string #x0085) "...")
                             (cons (char-to-string #x0091) "'")
                             (cons (char-to-string #x0092) "'")
                             (cons (char-to-string #x0093) "\"")
                             (cons (char-to-string #x0094) "\"")
                             (cons (char-to-string #x0095) "*")
                             (cons (char-to-string #x0096) "--")
                             (cons (char-to-string #x0097) "--")
                             (cons (char-to-string #x0099) "(tm)")
                             ;; The first ones above are apparently not
                             ;; official, but I know I've encountered them,
                             ;; and a DuckDuckGo search validates them.
                             (cons (char-to-string #x00a9) "(c)")
                             (cons (char-to-string #x00ae) "(r)")
                             (cons (char-to-string #x00b7) "*")

                             ;; Same as above but with some kooky extra
                             ;; leading bytes. I guess these come from UTF-8
                             ;; somehow. Emacs still shows these as single
                             ;; byte characters, so they're indistinguishable
                             ;; from the list above.
                             (cons (char-to-string #x3fff85) "...")
                             (cons (char-to-string #x3fff91) "'")
                             (cons (char-to-string #x3fff92) "'")
                             (cons (char-to-string #x3fff93) "\"")
                             (cons (char-to-string #x3fff94) "\"")
                             (cons (char-to-string #x3fff95) "*")
                             (cons (char-to-string #x3fff96) "--")
                             (cons (char-to-string #x3fffa9) "(c)")
                             (cons (char-to-string #x3fffae) "(r)")
                             (cons (char-to-string #x3fffb7) "*")

                             (cons (char-to-string #x2010) "-")
                             (cons (char-to-string #x2011) "-")
                             (cons (char-to-string #x2012) "-")
                             (cons (char-to-string #x2013) "-")
                             (cons (char-to-string #x2014) "--")
                             (cons (char-to-string #x2015) "--")
                             (cons (char-to-string #x2018) "'")
                             (cons (char-to-string #x2019) "'")
                             (cons (char-to-string #x201b) "'")
                             (cons (char-to-string #x201c) "\"")
                             (cons (char-to-string #x201d) "\"")
                             (cons (char-to-string #x201f) "\"")
                             (cons (char-to-string #x2026) "...")
                             (cons (char-to-string #x2032) "'")
                             (cons (char-to-string #x2033) "\"")
                             (cons (char-to-string #x2035) "'")
                             (cons (char-to-string #x2036) "\"")

                             (cons (char-to-string #x2120) "(sm)")
                             (cons (char-to-string #x2122) "(tm)")

                             ;; The rest don't show up in any reference
                             ;; search, but I know I've encountered them.
                             (cons (char-to-string #xd0c4) "...")
                             (cons (char-to-string #xd0c7) "'")
                             (cons (char-to-string #xd0c8) "\"")
                             (cons (char-to-string #xd0c9) "\"")
                             (cons (char-to-string #x53973) "-")
                             (cons (char-to-string #x53974) "--")
                             (cons (char-to-string #x53978) "'")
                             (cons (char-to-string #x53979) "'")
                             (cons (char-to-string #x5397c) "\"")
                             (cons (char-to-string #x5397d) "\"")))
        ;; Declaring this local variable causes the Emacs variable of the same
        ;; name to be preserved after this function completes.  save-excursion
        ;; doesn't cover this one.
        (deactivate-mark)
       )
    (save-excursion
      (while (not (null special-chars))
        (goto-char (point-min))     ; beginning of buffer
        (setq search-for (car (car special-chars)))
        (setq replace-with (cdr (car special-chars)))
        (while (search-forward search-for nil t)
          (replace-match replace-with nil t)
        )
        (setq special-chars (cdr special-chars))
      )
    )
  )
)

(defun replace-whitespace-linefeed ()
  "Replace one or more spaces with a line break throughout the current buffer."
  (interactive)
  (replace-regexp "  *" "\n" nil (point-min) (point-max))
)

(defun query-replace-multibuf-worker (replace-func from-string to-string)
  "Worker function for two different multibuf wrappers."
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer))
          (file (buffer-file-name buffer)))
      (switch-to-buffer buffer)
      (cond
       ;; Skip buffers whose names start with a space (i.e. internal buffers).
       ((string= (substring name 0 1) " "))
       ;; Skip read-only buffers
       (buffer-read-only)
       ;; Proceed only if this buffer has an associated file
       (file
        (funcall replace-func from-string to-string
                 nil (point-min) (point-max))
       )
      )
    )
  )
  ;; For consistency, show the user the buffer menu when done.
  (buffer-menu)
)

(defun query-replace-multibuf
  (from-string to-string &optional bogus1 bogus2 bogus3)
  "Wrapper for query-replace that applies to all writable buffers with
associated files.  The optional arguments are ignored; they exist in order to
call the wrapped function with minimal coding pain."
  ;; I lifted this undocumented use of 'interactive' from the source code to
  ;; query-replace.
  (interactive (query-replace-read-args "Query replace" nil))
  (query-replace-multibuf-worker 'query-replace from-string to-string)
)

(defun query-replace-regexp-multibuf
  (regexp to-string &optional bogus1 bogus2 bogus3)
  "Wrapper for query-replace-regexp that applies to all writable buffers with
associated files.  The optional arguments are ignored; they exist in order to
call the wrapped function with minimal coding pain."
  ;; I lifted this undocumented use of 'interactive' from the source code to
  ;; query-replace-regexp.
  (interactive (query-replace-read-args "Query replace regexp" t))
  (query-replace-multibuf-worker 'query-replace-regexp regexp to-string)
)

(global-set-key [?\C-s]       'isearch-forward)      ; already the default
(global-set-key [?\C-r]       'query-replace-regexp)
(global-set-key [?\M-r]       'query-replace)
(global-set-key [?\C-x ?\C-r] 'query-replace-regexp-multibuf)
(global-set-key [?\C-x ?\M-r] 'query-replace-multibuf)
(global-set-key [?\M-#]       'replace-special-chars)
(global-set-key [?\M-%]       'replace-whitespace-linefeed)
(global-set-key [?\M-&]       'delete-trailing-whitespace)
