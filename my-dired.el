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
;; Dired
;;
;; I need to override some funcs from this dired file.  Rather than mess with
;; autoload (which I don't think would work for one func I need), I just load
;; this entire file at startup.  I always launch dired anyway.
(load "dired-aux")

;; I'm redefining this dired func to customize it.
;; The only change is the use of my-read-shell-command.
(require 'my-read-shell-command "my-shell")
(defun dired-read-shell-command (prompt arg files)
  "Read a dired shell command prompting with PROMPT (using
my-read-shell-command).
ARG is the prefix arg and may be used to indicate in the prompt which
FILES are affected."
  (minibuffer-with-setup-hook
      (lambda ()
	(set (make-local-variable 'minibuffer-default-add-function)
	     'minibuffer-default-add-dired-shell-commands))
    (dired-mark-pop-up
     nil 'shell files
     #'my-read-shell-command
     (format prompt (dired-mark-prompt arg files))
     nil nil)
  )
)

;; I'm redefining this dired func to customize it.
;; The only change is the use of my-shell-command.
(require 'my-shell-command "my-shell")
(defun dired-run-shell-command (command)
  (let ((handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler
        (apply handler 'shell-command (list command))
      (my-shell-command command))
  )
  ;; Return nil for the sake of nconc in dired-bunch-files.
  nil
)

(defun dired-subshell ()
  "Launch a shell subprocess from the current dired directory in a
uniquely named buffer."
  (interactive)
  (save-match-data
    (let ((new-buf-name (concat "shell-" (buffer-name))))
      (switch-to-buffer-other-window (current-buffer))
      (shell)
      (and (string-match "<[0-9]+>\\'" new-buf-name)
           (setq new-buf-name (substring new-buf-name 0 (match-beginning 0))))
      (rename-buffer (generate-new-buffer-name new-buf-name))
      (force-mode-line-update)
    )
  )
)

(defun dired-abs-cur-file-new-kill ()
  "Make new entry at the head of the kill ring containing the absolute path
to the file on the current dired line."
  (interactive)
  (kill-new (dired-get-filename))
)

(defun dired-marked-files-new-kill ()
  "Make new entry at the head of the kill ring containing all the marked
file names in the current dired buffer."
  (interactive)
  (kill-new (dired-shell-stuff-it
             "*" (dired-get-marked-files t current-prefix-arg) nil
            )
  )
)

(defun dired-marked-files-append-kill ()
  "Append the entry at the head of the kill ring with all the marked
file names in the current dired buffer."
  (interactive)
  (kill-append (dired-shell-stuff-it
                " *" (dired-get-marked-files t current-prefix-arg) nil
               )
               nil
  )
)

(require 'my-shell-command "my-shell")
(require 'my-read-shell-command "my-shell")
(defun dired-run-file (abs-path-p)
  "Execute the current line's file name, and give the user a chance to edit
the command line.  With optional prefix arg, use the current line's absolute
path."
  (interactive "P")
  (my-shell-command (my-read-shell-command
                     "Shell command: "
                     (if (null abs-path-p)
                         (concat "./" (dired-get-filename 'no-dir))
                       (dired-get-filename)
                     )
                    )
  )
)

(defun my-dired-do-rename ()
  "Rename or move the file under the cursor in dired mode.
Prompts with the current name and location as the default."
  (interactive)
  (let* ((old-name (dired-get-filename 'no-dir))
         (abs-old-name (expand-file-name (concat (dired-current-directory)
                                                 old-name)))
         (new-name (expand-file-name
                    (read-file-name (format "Rename %s to : " old-name)
                                    (dired-current-directory) nil nil
                                    old-name))))
    (unless (string= abs-old-name new-name)
      (rename-file abs-old-name new-name 1)
      ;; Silently rename the visited file of any buffer visiting this file.
      (and (get-file-buffer abs-old-name)
           (with-current-buffer (get-file-buffer abs-old-name)
             (set-visited-file-name new-name nil t)
             )
           )
      (dired-remove-file abs-old-name)
      ;; See if it's an inserted subdir, and rename that, too.
      (dired-rename-subdir abs-old-name new-name)
      (revert-buffer)
    )
  )
)

(defun my-dired-sandbox (which-sandbox)
  "Launch dired in one of several pre-defined paths."
  (interactive "cNumber of sandbox directory to open: ")
  (dired (cond
          ((= which-sandbox ?1) "~/ws/ndiags1/src")
          ((= which-sandbox ?2) "~/ws/ndiags2/src")
          ((= which-sandbox ?3) "~/ws/ndiags3/src")
          ((= which-sandbox ?4) "~/ws/nconapp1/src")
          ((= which-sandbox ?5) "~/ws/nconapp2/src")
          ((= which-sandbox ?6) "~/ws/nconapp3/src")
          ((= which-sandbox ?!) "~/ws/ndiags1/obj")
          ((= which-sandbox ?@) "~/ws/ndiags2/obj")
          ((= which-sandbox ?#) "~/ws/ndiags3/obj")
          ((= which-sandbox ?$) "~/ws/nconapp1/obj")
          ((= which-sandbox ?%) "~/ws/nconapp2/obj")
          ((= which-sandbox ?^) "~/ws/nconapp3/obj")
         ))
)

(defun my-dired-diff (xxdiff-p)
  "Same as dired-diff, but prefix arg now means run xxdiff instead."
  (interactive "P")
  (let* ((current (dired-get-filename t))
         (init-default (if (mark t)
                           (save-excursion (goto-char (mark t))
                                           (dired-get-filename t t))))
         (default (unless (or (equal init-default current)
                              (and (not (equal (dired-dwim-target-directory)
                                               (dired-current-directory)))
                                   (not mark-active)
                              )
                          )
                    init-default))
         (file1 (read-file-name
                 (format "Diff %s with%s: " current
                         (if default (concat " (default " default ")") ""))
                 (if default
                     (dired-current-directory)
                   (dired-dwim-target-directory))
                 default t))
         (file2 (dired-get-filename t)))
    (if xxdiff-p
        (start-process "Diff" nil shell-file-name shell-command-switch
                       (format "xxdiff %s %s" file1 file2))
      (require 'diff)
      (diff file1 file2)
    )
    (deactivate-mark)
  )
)

;; Don't preserve last modified time stamp when copying files in dired.
;; Reason: it can make version control (esp. Accurev) miss modified files.
(setq dired-copy-preserve-time nil)

;; In dired, incremental search only file names when the cursor is on a file
;; name.  Only applies to Emacs 23, but doesn't seem to hurt anything to set
;; it in older versions.
(setq dired-isearch-filenames 'dwim)

;; Have dired guess intelligently at the destination directory for copy and
;; rename commands.  When the frame is split into multiple dired windows,
;; guess one of those directories.
(setq dired-dwim-target t)

;; Disable obnoxious marking of files in the destination dir's window after a
;; copy command.
(setq dired-keep-marker-copy nil)

;; The default time style doesn't seem to be the same on all Linux
;; distributions.  I have the same option in my "ll" alias created during bash
;; start-up.  Beware that the short options need to appear at the end because
;; some dired commands will cause "ls" to be run with other short options
;; appended without a leading space or dash.
(setq dired-listing-switches "--time-style=long-iso -al")

;; Use uni-diff with my preferred options instead of the default context diff.
;; Applies to dired and maybe other modes, too.
(setq diff-switches "-upN")

(add-hook 'dired-mode-hook
  (function (lambda ()
              (local-set-key [?\M-n]       'dired-next-line)
              (local-set-key [?\M-p]       'dired-previous-line)
              (local-set-key [?\C-c ?s]    'dired-subshell)
              (local-set-key [?\C-c ?\C-s] 'dired-subshell)
              (local-set-key [?\C-c ?\M-s] 'dired-subshell)
              (local-set-key [?\C-c ?w]    'dired-marked-files-append-kill)
              (local-set-key [?\C-c ?\M-c] 'dired-marked-files-new-kill)
              (local-set-key [?\C-c ?\C-w] 'dired-marked-files-new-kill)
              (local-set-key [?\C-c ?\M-w] 'dired-abs-cur-file-new-kill)
              (local-set-key [?\C-c ?\r]   'dired-run-file)
              (local-set-key [?r]          'my-dired-do-rename)
              (local-set-key [?=]          'my-dired-diff)
            )
  )
)

(global-set-key [f5] 'my-dired-sandbox)
