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

(when (file-accessible-directory-p my-3rd-party-elisp-path)
  (require 'dired-efap)
  (require 'dired-details+)    ; http://www.emacswiki.org/emacs/DiredDetails
)

(setq dired-details-hide-link-targets nil)

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

;; I don't understand the functional difference between call-process and
;; start-process, but gnome-open only works with call-process.
(defun my-dired-gnome-open ()
  "Call gnome-open on the current line's file name."
  (interactive)
  (call-process "gnome-open" nil 0 nil (dired-get-filename 'no-dir))
)

(setq dired-re-exec (concat dired-re-maybe-mark
                            dired-re-inode-size
                            "-[-r][-w]x"))
(defface dired-executable
  '((t (:inherit font-lock-keyword-face)))
  "Face used for executable files."
  :group 'dired-faces
  :version "22.1")
(defvar dired-executable-face 'dired-executable
  "Face name used for executable files.")

;; Add this face for executables to the ones dired already recognizes.
(add-to-list 'dired-font-lock-keywords
             (list dired-re-exec
                   '(".+" (dired-move-to-filename)
                     nil (0 dired-executable-face)))
             t)

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

;; Use uni-diff with my preferred options instead of the default context diff.
;; Applies to dired and maybe other modes, too.
(setq diff-switches "-upN")

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
;; start-up.  The short options need to appear at the end because of the
;; regexps below and because some older versions of Emacs appended other short
;; options without a leading space or dash.
(setq dired-listing-switches "--time-style=long-iso -al")

;; Redefine this dired function to match the custom sorting feature below.  We
;; don't attempt to use generic regular expressions.  This is hard-coded to
;; match the dired-listing-switches above, so the two must stay in sync when
;; editing this code.
(defun dired-sort-set-modeline ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
	  (let (case-fold-search)
	    (cond
             ((string-match "-al$" dired-actual-switches)
              "Dired by nm")
             ((string-match "-al --group-directories-first$"
                            dired-actual-switches)
              "Dired by nd1")
             ((string-match "-alX$" dired-actual-switches)
              "Dired by ext")
             ((string-match "-alS$" dired-actual-switches)
              "Dired by sz")
             ((string-match "-alt$" dired-actual-switches)
              "Dired by tm")
             (t
              (concat "Dired " dired-actual-switches))
            )
          )
    )
    (force-mode-line-update)
  )
)

;; Adapted from Patrick Anderson's file
;; http://www.emacswiki.org/emacs/dired-sort-map.el
(defun dired-sort-by-name ()
  "Sort dired listing by [n]ame"
  (interactive)
  (dired-sort-other dired-listing-switches)
)
(defun dired-sort-by-name-dirs-1st ()
  "Sort dired listing by name, grouping [d]irs first"
  (interactive)
  (dired-sort-other (concat dired-listing-switches
                            " --group-directories-first"))
)
(defun dired-sort-by-ext ()
  "Sort dired listing by e[x]tension"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X"))
)
(defun dired-sort-by-size ()
  "Sort dired listing by [s]ize"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S"))
)
(defun dired-sort-by-time ()
  "Sort dired listing by [t]ime"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t"))
)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "n" 'dired-sort-by-name)
(define-key dired-sort-map "d" 'dired-sort-by-name-dirs-1st)
(define-key dired-sort-map "x" 'dired-sort-by-ext)
(define-key dired-sort-map "s" 'dired-sort-by-size)
(define-key dired-sort-map "t" 'dired-sort-by-time)

(add-hook 'dired-mode-hook
  (function (lambda ()
              (local-set-key [?\C-c ?w]    'dired-marked-files-append-kill)
              (local-set-key [?\C-c ?\C-w] 'dired-marked-files-new-kill)
              (local-set-key [?\C-c ?\M-w] 'dired-abs-cur-file-new-kill)
              (local-set-key [?\C-c ?\r]   'dired-run-file)
              (local-set-key [?\M-x ?\M-q] 'dired-toggle-read-only)
              (local-set-key [C-return]    'my-dired-gnome-open)
              (local-set-key [?\C-j]       'my-dired-gnome-open)
              (local-set-key [?r]          'dired-efap)
              (local-set-key [?=]          'my-dired-diff)
              (dired-sort-by-name-dirs-1st)
            )
  )
)

(global-set-key [f5]          'my-dired-sandbox)
(global-set-key [?\M-g ?\M-w] 'my-dired-sandbox)
