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

(defvar my-dired-preview-p nil)
(make-variable-buffer-local 'my-dired-preview-p)
(defvar my-dired-max-preview-lines 2000)
(defvar my-dired-max-preview-bytes 1000000)

(defun my-preview-worker (cmd outbuf)
  "Worker function for file previews generated by running a command."
  (with-current-buffer outbuf
    (erase-buffer)
    (shell-command cmd outbuf)
    ;; Hack for unwanted weird space characters in output of tree command.
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (char-to-string #xa0) nil t)
        (replace-match " " nil t)
      )
    )
    (text-mode)
    (toggle-truncate-lines 0)
  )
)

(defun my-preview-tar-handler (outbuf full-path notdir)
  (my-preview-worker (format "tar tf \"%s\" | sort | head -n%d"
                             notdir my-dired-max-preview-lines)
                     outbuf)
)

(defun my-preview-7zip-handler (outbuf full-path notdir)
  (my-preview-worker (format "7z l \"%s\" | head -n%d"
                             notdir my-dired-max-preview-lines)
                     outbuf)
)

(defun my-preview-rar-handler (outbuf full-path notdir)
  (my-preview-worker (format "rar l \"%s\" | head -n%d"
                             notdir my-dired-max-preview-lines)
                     outbuf)
)

(defun my-preview-deb-handler (outbuf full-path notdir)
  (my-preview-worker (format "dpkg --info \"%s\"" notdir) outbuf)
)

(defun my-preview-json-handler (outbuf full-path notdir)
  (my-preview-worker (format "jq . \"%s\" | head -n%d"
                             notdir my-dired-max-preview-lines)
                     outbuf)
  (js-mode)
  (toggle-truncate-lines 1)
)

(defvar my-preview-handler-alist
  '(("\\.tar\\(\\.[^.]+\\)?\\'" . my-preview-tar-handler)
    ("\\.\\(tgz\\|tbz\\|tbz2\\|txz\\|tzst\\)\\'" . my-preview-tar-handler)
    ("\\.json\\'" . my-preview-json-handler)
    ("\\.7z\\'" . my-preview-7zip-handler)
    ("\\.rar\\'" . my-preview-rar-handler)
    ("\\.deb\\'" . my-preview-deb-handler)))

(defun my-dired-preview-file ()
  "View the file at the dired point in the other window, but preview it.
That means load the file into a special buffer named *Dired file preview*,
and detach it from the underlying file.
"
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (dired-dir default-directory)
         (notdir (file-name-nondirectory filename))
         (handler (assoc-default notdir my-preview-handler-alist
                                 'string-match))
         (encoding (string-trim
                    (shell-command-to-string
                     (format "file -Lb --mime-encoding \"%s\"" filename))))
         (mime-type (string-trim
                     (shell-command-to-string
                      (format "file -Lb --mime-type \"%s\"" filename))))
         (outbuf (get-buffer-create "*Dired file preview*"))
         (inhibit-message t)
         (inhibit-read-only t))
    (with-current-buffer outbuf
      ;; This buffer could have been last used by a dired in a different dir.
      (setq default-directory dired-dir)
      (read-only-mode 0)
      (cond
       ((file-directory-p filename)
        (my-preview-worker (format "tree -L 3 -F \"%s\" | head -n%d"
                                   notdir my-dired-max-preview-lines)
                           outbuf))
       ((member-if (lambda (x) (string= x mime-type))
                   '("application/zip" "application/java-archive"))
        (my-preview-worker (format "zipinfo -1 \"%s\" | sort | head -n%d"
                                   notdir my-dired-max-preview-lines)
                           outbuf))
       (handler
        (funcall handler outbuf filename notdir))
       ((and (string= encoding "binary")
             (not (string= (substring mime-type 0 6) "image/")))
        (my-preview-worker (format "file -b \"%s\" ; mediainfo \"%s\""
                                   notdir notdir)
                           outbuf))
       (t  ; load and detach
        (insert-file-contents filename nil 0 my-dired-max-preview-bytes t)
        (setq buffer-file-name filename)
        (set-auto-mode)
        (setq buffer-file-name nil)
        (toggle-truncate-lines 1))
      )
      (set-buffer-modified-p nil)
      (read-only-mode 1)
      (display-buffer outbuf t)
    )
  )
)

(defun my-toggle-dired-preview ()
  (interactive)
  (setq my-dired-preview-p (not my-dired-preview-p))
  (if my-dired-preview-p (my-dired-preview-file))
)

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
is enabled and displays a preview of the file in the other window if
my-dired-preview-p is enabled."
  (interactive "p")
  (when (dired-next-line arg)     ; returns nil when cur line isn't a dir entry
    (if my-dired-preview-p (my-dired-preview-file))
    (if dired-hide-details-mode (my-dired-details-message))
  )
)

(defun my-dired-previous-line (arg)
  "Wrapper for \\[dired-previous-line] that afterward displays the
current entry's details in the minibuffer if dired-hide-details-mode
is enabled and displays a preview of the file in the other window if
my-dired-preview-p is enabled."
  (interactive "p")
  (when (dired-previous-line arg) ; returns nil when cur line isn't a dir entry
    (if my-dired-preview-p (my-dired-preview-file))
    (if dired-hide-details-mode (my-dired-details-message))
  )
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

(defun my-dired-find-file (which-window)
  "My implementation of dired-find-file with extra features.
First prepends current file to file-name-history.
Arg which-window must be one of the following three symbols:
  'cur:            current window
  'other:          other window
  'other-but-stay: other window, but leave current window selected"
  ;; Set find-file-run-dired so that the command works on directories too,
  ;; independent of the user's setting.
  (let ((find-file-run-dired t)
        (tgt (dired-get-file-for-visit)))
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

;; Beware: New option dired-kill-when-opening-new-dired-buffer in v28 is
;; similar to my feature for replacing the current dired buffer with the new
;; one.
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
        (call-process
         (if (string-equal system-type "darwin") "open" "xdg-open")
         nil 0 nil (dired-get-filename 'no-dir))
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

(defun my-dired-jump (oth-window-p)
  "Wrapper for dired-jump that uses an optional prefix to call
dired-jump-other-window instead."
  (interactive "P")
  (if (null oth-window-p)
      (dired-jump)
    (dired-jump-other-window)
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

(when (string-equal system-type "darwin")
  ;; From package exec-path-from-shell. For gls below, but also useful in
  ;; general.
  (exec-path-from-shell-initialize)
  ;; Provided by coreutils package in homebrew. Needed for the --dired option.
  (setq insert-directory-program "gls")
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
              (local-set-key [?\M-^]       (lambda () (interactive)
                                             (my-dired-up-directory t)))
              (local-set-key [?^]          'my-dired-up-directory)
              (local-set-key [C-return]    'my-dired-xdg-open)
              (local-set-key [?\C-j]       'my-dired-xdg-open)
              (local-set-key [?r]          'dired-efap)
              (local-set-key [?=]          'my-dired-diff)
              (local-set-key [?y]          'my-dired-show-file-info)
              (local-set-key [?i]          'my-dired-preview-file)
              (local-set-key [?I]          'my-toggle-dired-preview)
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
              (recentf-push default-directory)
            )
  )
)

(defhydra my-dired-sandbox (:color blue)
  "
Open directory in dired:
_t_ ~/tmp
"
  ("t" (progn (dired "~/tmp") (rename-buffer "tmp")) nil)
)

(global-set-key [?\C-x ?\C-j] 'my-dired-jump)
(global-set-key [?\C-x ?\M-j] 'my-dired-jump)

(global-set-key [f5]          'my-dired-sandbox/body)
;; Extra bindings for MacOS because of the infernal touch bar.
(global-set-key [?\M-g ?5]    'my-dired-sandbox/body)
(global-set-key [?\M-g ?\M-w] 'my-dired-sandbox/body)
