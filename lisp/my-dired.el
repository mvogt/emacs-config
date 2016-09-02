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
(require 'dired-aux)
(require 'dired-efap)

(defun my-dired-details-message ()
  "Print the current dired entry's details in the minibuffer."
  (interactive)
  (save-excursion
    (let* ((start (+ 2 (progn (beginning-of-line) (point))))
           (end (progn (dired-move-to-filename) (point)))
           (txt (buffer-substring start end)))
      (set-text-properties 0 (length txt) nil txt)
      (message txt)
    )
  )
)

(defun my-dired-next-line (arg)
  "Wrapper for \\[dired-next-line] that afterward displays the
current entry's details in the minibuffer if dired-hide-details-mode
is enabled."
  (interactive "p")
  (and (dired-next-line arg)     ; returns nil when cur line isn't a dir entry
       dired-hide-details-mode
       (my-dired-details-message))
)

(defun my-dired-previous-line (arg)
  "Wrapper for \\[dired-previous-line] that afterward displays the
current entry's details in the minibuffer if dired-hide-details-mode
is enabled."
  (interactive "p")
  (and (dired-previous-line arg) ; returns nil when cur line isn't a dir entry
       dired-hide-details-mode
       (my-dired-details-message))
)

(defun my-dired-revert (&optional arg noconfirm)
  "Wrapper for \\[dired-revert] that afterward displays the
current entry's details in the minibuffer if dired-hide-details-mode
is enabled."
  (interactive)
  (dired-revert arg noconfirm)
  (and (dired-move-to-filename)  ; returns nil when cur line isn't a dir entry
       dired-hide-details-mode
       (my-dired-details-message))
)

(defun my-dired-show-file-info (file &optional deref-symlinks)
  "Same as \\[dired-show-file-type] but also runs stat."
  (interactive (list (dired-get-filename t) current-prefix-arg))
  (let (process-file-side-effects)
    (with-temp-buffer
      (if deref-symlinks
	  (process-file "file" nil t t "-L" "--" file)
	(process-file "file" nil t t "--" file))
      (process-file "stat" nil t t "--" file)
      (when (bolp)
	(backward-delete-char 1))
      (message "%s" (buffer-string))
    )
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
(defun my-dired-run-file (abs-path-p)
  "Execute the current line's file name, and give the user a chance to edit
the command line.  With optional prefix arg, use the current line's absolute
path."
  (interactive "P")
  (my-shell-command (read-shell-command
                     "Shell command: "
                     (if (null abs-path-p)
                         (concat "./" (dired-get-filename 'no-dir))
                       (dired-get-filename)
                     )
                    )
  )
)

(defun path-to-tilde (path)
  "If path start with $HOME, replace $HOME with ~."
  (let* ((homedir (getenv "HOME"))
         (home-len (length homedir)))
    (if (and homedir
             (> (length path) home-len)
             (string= homedir (substring path 0 home-len)))
        (concat "~" (substring path home-len))
      path
    )
  )
)

(defun dropbox-p (path)
  "If path is a file and starts with $HOME/Dropbox, return t, else nil.
Also handles the case where path is a symlink to somewhere in the dropbox."
  (let* ((dropbox (concat (getenv "HOME") "/Dropbox"))
         (dbox-len (length dropbox))
         (resolved-path (file-truename path)))
    (and (not (file-directory-p resolved-path))
         (> (length resolved-path) dbox-len)
         (string= dropbox (substring resolved-path 0 dbox-len)))
  )
)

(defun my-dired-find-file (which-window)
  "My implementation of dired-find-file with extra features.
First prepends current file to file-name-history.
Arg which-window must be one of the following three symbols:
  'cur:            current window
  'other:          other window
  'other-but-stay: other window, but leave current window selected
If the path to edit is a file and under ~/Dropbox, use wrapper
script edit-dbox instead of opening the file directly. This skips
modifying file-name-history because the whole point of the
wrapper is to never accidentally open the original file.
Unfortunately for now, because the file gets opened through a
child process call to emacsclient, this overrides which-window to
'cur."
  ;; Set find-file-run-dired so that the command works on directories too,
  ;; independent of the user's setting.
  (let ((find-file-run-dired t)
        (tgt (dired-get-file-for-visit)))
    (if (dropbox-p tgt)
        (start-process "edit-dbox" nil "edit-dbox" tgt)
      (setq file-name-history (cons (path-to-tilde tgt) file-name-history))
      (cond
       ((eq which-window 'cur)
        (find-file tgt))
       ((eq which-window 'other)
        (find-file-other-window tgt))
       ((eq which-window 'other-but-stay)
        (display-buffer (find-file-noselect tgt)))
      )
    )
  )
)

(defun my-dired-xdg-open ()
  "Call xdg-open on the current line's file name.
If it's a directory, open a new dired buffer, and kill the current one."
  (interactive)
  ;; Bind find-file-run-dired so that the command works on directories, too,
  ;; independent of the user's setting.
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (if (not (file-directory-p file))
        ;; I don't understand the functional difference between call-process
        ;; and start-process, but when I first wrote this with gnome-open, it
        ;; only worked with call-process. I didn't recheck later.
        (call-process "xdg-open" nil 0 nil (dired-get-filename 'no-dir))
      (setq file-name-history (cons (path-to-tilde file) file-name-history))
      (kill-buffer)
      (find-file file)
    )
  )
)

(defun my-dired-up-directory (&optional kill-buf-p)
  "Run Dired on parent directory of current directory.
With a prefix argument, kills the current buffer."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (setq file-name-history (cons (path-to-tilde up) file-name-history))
          (if kill-buf-p
            (kill-buffer))
          (dired up)
          (dired-goto-file dir)
        )
    )
  )
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
  (let ((sandbox (cond
                  ((= which-sandbox ?1) '("~/ws/nconapp1/src" "conapp1-src"))
                  ((= which-sandbox ?2) '("~/ws/nconapp2/src" "conapp2-src"))
                  ((= which-sandbox ?3) '("~/ws/nconapp3/src" "conapp3-src"))
                  ((= which-sandbox ?4) '("~/ws/nconapp4/src" "conapp4-src"))
                  ((= which-sandbox ?5) '("~/ws/nconapp5/src" "conapp5-src"))
                  ((= which-sandbox ?6) '("~/ws/nconapp6/src" "conapp6-src"))
                  ((= which-sandbox ?7) '("~/ws/nconapp7/src" "conapp7-src"))
                  ((= which-sandbox ?8) '("~/ws/nconapp8/src" "conapp8-src"))
                  ((= which-sandbox ?!) '("~/ws/nconapp1/obj" "conapp1-obj"))
                  ((= which-sandbox ?@) '("~/ws/nconapp2/obj" "conapp2-obj"))
                  ((= which-sandbox ?#) '("~/ws/nconapp3/obj" "conapp3-obj"))
                  ((= which-sandbox ?$) '("~/ws/nconapp4/obj" "conapp4-obj"))
                  ((= which-sandbox ?%) '("~/ws/nconapp5/obj" "conapp5-obj"))
                  ((= which-sandbox ?^) '("~/ws/nconapp6/obj" "conapp6-obj"))
                  ((= which-sandbox ?&) '("~/ws/nconapp7/obj" "conapp7-obj"))
                  ((= which-sandbox ?*) '("~/ws/nconapp8/obj" "conapp8-obj"))
                  ((= which-sandbox ?t) '("~/tmp"             "tmp"))
                 )))
    (dired (car sandbox))
    (rename-buffer (cadr sandbox))
  )
)

(defun my-dired-diff (gui-p)
  "Same as dired-diff, but prefix arg now means run meld instead."
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
    (if gui-p
        (start-process "Diff" nil shell-file-name shell-command-switch
                       (format "meld %s %s" file1 file2))
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

;; Brief listings should still show the target of symlinks and info lines.
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-hide-details-hide-information-lines nil)

;; The default time style doesn't seem to be the same on all Linux
;; distributions.  I have the same option in my "ll" alias created during bash
;; start-up.  The short options need to appear at the end because of the
;; regexps below and because some older versions of Emacs appended other short
;; options without a leading space or dash.
(setq dired-listing-switches "--time-style=long-iso -ahl")

;; Redefine this dired function to match the custom sorting feature below.  We
;; don't attempt to use generic regular expressions.  This is hard-coded to
;; match the dired-listing-switches above, so the two must stay in sync when
;; editing this code.
(defun dired-sort-set-mode-line ()
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond
             ((string-match "-ahl$" dired-actual-switches)
              "Dired by nm")
             ((string-match "-ahl --group-directories-first$"
                            dired-actual-switches)
              "Dired by nd1")
             ((string-match "-ahlX$" dired-actual-switches)
              "Dired by ext")
             ((string-match "-ahlS$" dired-actual-switches)
              "Dired by sz")
             ((string-match "-ahlt$" dired-actual-switches)
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
              (setq autopair-dont-activate t)
              (setq revert-buffer-function (function my-dired-revert))
              (local-set-key [?\C-c ?w]    'dired-marked-files-append-kill)
              (local-set-key [?\C-c ?\C-w] 'dired-marked-files-new-kill)
              (local-set-key [?\C-c ?\M-w] 'dired-abs-cur-file-new-kill)
              (local-set-key [?\C-c ?\r]   'my-dired-run-file)
              (local-set-key [?\M-x ?\M-q] 'dired-toggle-read-only)
              (local-set-key [C-backspace] (lambda () (interactive)
                                             (my-dired-up-directory t)))
              (local-set-key [?^]          'my-dired-up-directory)
              (local-set-key [C-return]    'my-dired-xdg-open)
              (local-set-key [?\C-j]       'my-dired-xdg-open)
              (local-set-key [?r]          'dired-efap)
              (local-set-key [?=]          'my-dired-diff)
              (local-set-key [?y]          'my-dired-show-file-info)
              (local-set-key [?n]          'my-dired-next-line)
              (local-set-key [?\C-n]       'my-dired-next-line)
              (local-set-key [? ]          'my-dired-next-line)
              (local-set-key [down]        'my-dired-next-line)
              (local-set-key [?p]          'my-dired-previous-line)
              (local-set-key [?\C-p]       'my-dired-previous-line)
              (local-set-key [up]          'my-dired-previous-line)
              (local-set-key [return]      (lambda () (interactive)
                                             (my-dired-find-file 'cur)))
              (local-set-key [?f]          (lambda () (interactive)
                                             (my-dired-find-file 'cur)))
              (local-set-key [?e]          (lambda () (interactive)
                                             (my-dired-find-file 'cur)))
              (local-set-key [?o]          (lambda () (interactive)
                                             (my-dired-find-file 'other)))
              (local-set-key [?\C-o]       (lambda () (interactive)
                                             (my-dired-find-file
                                              'other-but-stay)))
              (dired-sort-by-name-dirs-1st)
              (dired-hide-details-mode t)
            )
  )
)

(global-set-key [f5]          'my-dired-sandbox)
(global-set-key [?\M-g ?\M-w] 'my-dired-sandbox)
