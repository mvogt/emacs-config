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
(defvar my-recursive-grep-cmd-hist '())
(defun my-recursive-grep-worker (which-func)
  "Search all files recursively.
Like grep-find, but prompts for a starting directory, guesses the search
string from context, and adds my favorite find and grep options.
Intended to be called from a hydra with the type of search."
  (interactive)
  (let* ((my-xargs-grep (concat "xargs -0 grep --line-number"
                                " --ignore-case --no-messages --color=auto"
                                " --extended-regexp -e"))
         (find-args-ignore "-o -name .git -prune -o -name .svn -prune")
         (cmd-spec
          (cond
           ((eq which-func 'all)
            (list 'grep-mode
                  (format "find . -type f -print0 %s \\\n| %s \"%s\""
                          find-args-ignore my-xargs-grep
                          (my-cur-word-or-region))))
           ((eq which-func 'ext)
            (list 'grep-mode
                  (format (concat "find . -type f -name \"*.c\" -print0 "
                                  find-args-ignore " \\\n| %s \"%s\"")
                          my-xargs-grep (my-cur-word-or-region))))
           ((eq which-func 'multi)
            (list 'grep-mode
                  (format (concat "find . -type f -name \"*.c\" -print0"
                                  " -o -name \"*.h\" -print0 "
                                  find-args-ignore " \\\n| %s \"%s\"")
                          my-xargs-grep (my-cur-word-or-region))))
           ((eq which-func 'fnames)
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
         (my-grep-cmd (read-string "Search command: " (cdr cmd-spec)
                                   'my-recursive-grep-cmd-hist))
         (start-dir (read-file-name "Starting directory: "
                                    nil default-directory))
        )
    (setenv "colon" ":")
    ;; grep has recently started printing a warning about GREP_OPTIONS
    ;; being deprecated, but Emacs v24 still uses it unconditionally to
    ;; set --color. My work-around is to unset it here and specify --color
    ;; in my command.
    (compilation-start (concat "cd " start-dir " && unset GREP_OPTIONS ; "
                               my-grep-cmd)
                       (car cmd-spec))
  )
)

;; https://github.com/abo-abo/hydra
(defhydra my-recursive-grep (:color blue)
  "
Recursive file search:
_x_ one extension                       _g_ ag (Silver Searcher)
_m_ multiple extensions                 _j_ just file names
_a_ all files with find | xargs grep
"
  ("x" (my-recursive-grep-worker 'ext) nil)
  ("m" (my-recursive-grep-worker 'multi) nil)
  ("a" (my-recursive-grep-worker 'all) nil)
  ("g" (my-recursive-grep-worker 'ag) nil)
  ("j" (my-recursive-grep-worker 'fnames) nil)
)

;; The extra bindings are for MacOS because of the infernal touch bar.
(global-set-key [f4]          'my-recursive-grep/body)
(global-set-key [?\M-g ?4]    'my-recursive-grep/body)
(global-set-key [?\M-g ?\M-4] 'my-recursive-grep/body)
(global-set-key [?\M-g ?\M-s] 'my-recursive-grep/body)
;; For find-tag, must do visit-tags-table first (once per session).
(global-set-key [C-f4]        'find-tag)
;; For grep and compile buffers (or anything in compilation-mode).
(global-set-key [f6]          'next-error)
(global-set-key [f8]          'gdb)
(global-set-key [?\M-g ?8]    'gdb)
(global-set-key [?\M-g ?\M-8] 'gdb)
(global-set-key [f9]          'compile)
(global-set-key [?\M-g ?9]    'compile)
(global-set-key [?\M-g ?\M-9] 'compile)

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
