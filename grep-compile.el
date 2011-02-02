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

(autoload 'compile-internal "compile")
(defun my-recursive-grep (variant)
  "Search all files recursively.
Like grep-find, but prompts for a starting directory, guesses the search
string from context, and adds my favorite find and grep options.  A C-u
prefix changes the file name match to *.c, and a C-u C-u prefix changes
the search to a find without grep."
  (interactive "P")
  (setenv "colon" ":")
  (let* ((cmd-prefix (if (null variant)
                         "find . -type f"
                         "find . -type f -name \"*.c\""))
         (my-initial-grep-cmd
          (if (= (prefix-numeric-value variant) 16)
              (format (concat "find . -type f -name \"%s\" -print"
                              " -o -name .git -prune | while read x ; do"
                              " echo ${x}${colon}1${colon} ; done")
                      (my-cur-word-or-region))
            (format (concat "%s -print0 -o -name .git -prune"
                            " | xargs --null"
                            " grep --line-number --ignore-case"
                            " --no-messages --extended-regexp -e \"%s\"")
                    cmd-prefix (my-cur-word-or-region))
          )
         )
         (my-grep-cmd (read-string "Search command: " my-initial-grep-cmd))
         (start-dir (read-file-name "Starting directory: "
                                    nil default-directory)))
    (if (>= emacs-major-version 22)
        ;; Much easier, simpler, and better in ver 22.  I may want to change
        ;; my key binding to just rgrep in ver 22 instead of this function.
        ;; However, I like my sequence of prompts better.
        (compilation-start (concat "cd " start-dir " && " my-grep-cmd)
                           'grep-mode)
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

(global-set-key [f9]   'compile)
(global-set-key [f7]   'my-recursive-grep)
;; For find-tag, must do visit-tags-table first (once per session).
(global-set-key [C-f7] 'find-tag)
;; For grep and compile buffers (or anything in compilation-mode).
(global-set-key [f6]   'next-error)

(add-hook 'compilation-mode-hook
  (function (lambda ()
              (local-unset-key [?g])    ; obnoxious
            )
  )
)
