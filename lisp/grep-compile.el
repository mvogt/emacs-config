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
;; Grep and compile
;;
;; Highlighting of grep matches within each line of grep output.  Works by
;; setting shell environment variables that tell the child grep process to
;; highlight matches within each line using ANSI escape sequences.  Applies to
;; grep mode in the built-in grep.el.  Introduced in Emacs 22, where it never
;; worked correctly for me.  (The raw escape code chars appeared in the Emacs
;; buffer containing the grep output.)  Seems to be fixed in Emacs 23.
(setq grep-highlight-matches (if (>= emacs-major-version 23) t nil))

;; Same highlighting within a line for ag ("The Silver Searcher") results.
(require 'ag)
(setq ag-highlight-search t)

;; Generate a unique buffer name for each process launched in compilation
;; mode.  This applies to the commands compile, grep, grep-find, and
;; my-recursive-grep.
(setq compilation-buffer-name-function
      (function (lambda (mode)
                  (generate-new-buffer-name
                   (concat "*" (downcase mode) "*")))))

;; My preferred default compile command (from my ~/bin)
(setq compile-command "build")

(defun my-cur-word-or-region ()
  "Return the current region, if active, else the word surrounding the point."
  (if mark-active
      (buffer-substring (point) (mark))
    (let (word)
      (save-excursion
        ;; Look for any word the cursor is on, or if the cursor isn't on a
        ;; word, then use the nearest preceding word.
        (setq word (or (current-word) ""))
        (if (string-match "[._]+$" word)
            (setq word (substring word 0 (match-beginning 0))))
      )
      word
    )
  )
)
(provide 'my-cur-word-or-region)

(autoload 'compile-internal "compile")
(defun my-recursive-grep ()
  "Search all files recursively.
Like grep-find, but prompts for a starting directory, guesses the search
string from context, and adds my favorite find and grep options.
First prompts for the type of files search."
  (interactive)
  (message (concat "Search [a]ll files, for one e[x]tension,"
                   " for [m]ultiple extensions,"
                   "\n[j]ust file names, or with a[g]?"))
  (let* ((my-xargs-grep (concat "xargs -0 grep --line-number"
                                " --ignore-case --no-messages --color=auto"
                                " --extended-regexp -e"))
         (find-args-ignore "-o -name .git -prune -o -name .svn -prune")
         (which-func (read-char))
         (cmd-spec
          (cond
           ((= which-func ?a)
            (list 'grep-mode
                  (format "find . -type f -print0 %s \\\n| %s \"%s\""
                          find-args-ignore my-xargs-grep
                          (my-cur-word-or-region))))
           ((= which-func ?x)
            (list 'grep-mode
                  (format (concat "find . -type f -name \"*.c\" -print0 "
                                  find-args-ignore " \\\n| %s \"%s\"")
                          my-xargs-grep (my-cur-word-or-region))))
           ((= which-func ?m)
            (list 'grep-mode
                  (format (concat "find . -type f -name \"*.c\" -print0"
                                  " -o -name \"*.h\" -print0 "
                                  find-args-ignore " \\\n| %s \"%s\"")
                          my-xargs-grep (my-cur-word-or-region))))
           ((= which-func ?j)
            (list 'grep-mode
                  (format (concat "find . -type f -name \"%s\" -print "
                                  find-args-ignore " | while read x ; do"
                                  " echo ${x}${colon}1${colon} ; done")
                          (my-cur-word-or-region))))
           (t
            (list 'ag-mode
                  (format (concat "ag --literal --depth 50 --smart-case"
                                  " --group --line-number --column --color"
                                  " --color-match \"30;43\" --color-path"
                                  " \"1;32\" -- \"%s\"")
                          (my-cur-word-or-region))))
          )
         )
         (my-grep-cmd (read-string "Search command: " (cdr cmd-spec)))
         (start-dir (read-file-name "Starting directory: "
                                    nil default-directory))
        )
    (setenv "colon" ":")
    (if (>= emacs-major-version 22)
        ;; Much easier, simpler, and better starting in ver 22.  I may want to
        ;; change my key binding to just rgrep in ver 22 instead of this
        ;; function.  However, I like my sequence of prompts better.
        ;;
        ;; grep has recently started printing a warning about GREP_OPTIONS
        ;; being deprecated, but Emacs v24 still uses it unconditionally to
        ;; set --color. My work-around is to unset it here and specify --color
        ;; in my command.
        (compilation-start (concat "cd " start-dir " && unset GREP_OPTIONS ; "
                                   my-grep-cmd)
                           (car cmd-spec))
      (let* ((corrected-start-dir (if (string= "/" (substring start-dir -1))
                                      start-dir
                                    (concat start-dir "/")))
                                        ; must have trailing slash
             ;; We're not supposed to set default-directory directly, but
             ;; that's the only way to influence the starting point of
             ;; compile-internal.  This may break in a future version of
             ;; Emacs.
             (default-directory corrected-start-dir)
             (compilation-process-setup-function 'grep-process-setup))
        (compile-internal my-grep-cmd "No more grep hits" "grep" nil
                          grep-regexp-alist)
      )
    )
  )
)

(global-set-key [f9]          'compile)
(global-set-key [?\C-x ?7]    'compile)
(global-set-key [f4]          'my-recursive-grep)
(global-set-key [?\M-g ?\M-s] 'my-recursive-grep)
;; For find-tag, must do visit-tags-table first (once per session).
(global-set-key [C-f4] 'find-tag)
;; For grep and compile buffers (or anything in compilation-mode).
(global-set-key [f6] 'next-error)

(add-hook 'compilation-mode-hook
  (function (lambda ()
              (local-unset-key [?g])    ; obnoxious
            )
  )
)
;; Prevent accidental changes to files visited through grep or compile mode.
(add-hook
 'next-error-hook
 (function
  (lambda ()
    (if (>= emacs-major-version 24) (read-only-mode 1) (toggle-read-only 1))
  )
 )
)
