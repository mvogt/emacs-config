;;
;; Copyright 2020  Mark Vogt
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
;; Helm
;;
(require 'helm)
(load "helm-autoloads")
(require 'helm-find)
(require 'helm-bookmark)
(require 'helm-org)
(setq bookmark-save-flag 1)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-split-window-default-side 'right)

;; This function is hard to describe. It basically means do the intuitive
;; thing. For some reason, the default for this is C-z.
(define-key helm-map [tab]   'helm-execute-persistent-action)
(define-key helm-map [?\C-i] 'helm-execute-persistent-action)
;; Most helm sessions have a choice of different actions you can perform on
;; your selected files. This toggles to a menu showing those action choices.
(define-key helm-map [backtab] 'helm-select-action)

;; I don't like the default C-o and M-o for this, and I override C-o in
;; several places to mean open in other window.
(define-key helm-map [?\M-N] 'helm-next-source)
(define-key helm-map [?\M-P] 'helm-previous-source)

(helm-mode 1)

;; Prevent helm from activating on any of these actions.
(cl-loop for trigger in '((dired-do-rename)
                          (dired-do-copy)
                          (dired-do-symlink)
                          (dired-do-relsymlink)
                          (dired-do-hardlink)
                          (write-file)
                          (basic-save-buffer))
         do (add-to-list 'helm-completing-read-handlers-alist trigger))

(defun my-call-with-prefix (pfx func)
  (let ((current-prefix-arg pfx))
    (call-interactively func)
  )
)

;;
;; Key assignments common to multiple helm session types
;;

;; To open the selection in the other window, C-o is much easier to type than
;; the default "C-c o". I left the default in place. Also, I always want to
;; call these functions with the universal prefix to cause a vertical split
;; rather than a horizontal one.
(define-key helm-find-files-map    [?\C-o]
  (lambda () (interactive)
    (my-call-with-prefix '(4) 'helm-ff-run-switch-other-window)))
(define-key helm-generic-files-map [?\C-o]
  (lambda () (interactive)
    (my-call-with-prefix '(4) 'helm-ff-run-switch-other-window)))
(define-key helm-buffer-map        [?\C-o]
  (lambda () (interactive)
    (my-call-with-prefix '(4) 'helm-buffer-switch-other-window)))
(define-key helm-bookmark-map      [?\C-o]
  (lambda () (interactive)
    (my-call-with-prefix '(4) 'helm-bookmark-run-jump-other-window)))

;; The default mapping is C-c X. I haven't overwritten that.
(define-key helm-find-files-map    [?\C-c ?\C-j]
  'helm-ff-run-open-file-with-default-tool)
(define-key helm-generic-files-map [?\C-c ?\C-j]
  'helm-ff-run-open-file-with-default-tool)

;;
;; helm-find-files config
;;

;; To search paths below the current location, C-s is much more intuitive than
;; the default "C-c /". The original function that was on C-s is still
;; available on "M-g s".
(define-key helm-find-files-map [?\C-s] 'helm-ff-run-find-sh-command)

;; The default mapping for C-/ is helm-ff-run-fd, but I don't use the fd
;; command. I want the standard undo key to work as expected.
(define-key helm-find-files-map [?\C-/] 'helm-ff-undo)

;; The default mapping is C-x r b. I haven't overwritten that. It doesn't
;; appear in the action menu by default.
(define-key helm-find-files-map
  [?\C-c ?\C-b] 'helm-find-files-switch-to-bookmark)
(define-key helm-find-files-map
  [M-f3]        'helm-find-files-switch-to-bookmark)

;; The default mapping is C-x r m. I haven't overwritten that. It doesn't
;; appear in the action menu by default, and I haven't figured out a good way
;; to add it. So, I put a keyboard hint in the bookmark action created above.
(define-key helm-find-files-map [?\C-c ?\C-m] 'helm-ff-bookmark-set)

;; The default mapping is C-c h. I haven't overwritten that.
;; The default for M-r is rotate a viewed image file right. (M-l rotates
;; left.) I don't have that rotation feature working, and I'd never use it.
(define-key helm-find-files-map [?\M-r] 'helm-ff-file-name-history)
(define-key helm-find-files-map [M-f1]  'helm-ff-file-name-history)

;; I'm using M-F1,F2,F3 as an additional way to access the different kinds of
;; history available from the file finder. That's easier for me to remember.
(define-key helm-find-files-map [M-f2]  'helm-find-files-history)

;; I didn't know about helm-kill-selection-and-quit when I wrote this.
;; Since mine loops over the marked files and supports abbreviating with
;; tilde, I'm keeping it.
(defun my-helm-ff-copy-name (_candidate)
  "Copy names of marked files to kill ring.
With one universal prefix, use the full path names.
With two universal prefixes, abbreviate the full paths with ~ where possible."
  (with-helm-current-buffer
    (kill-new (mapconcat (lambda (cur)
                           (helm-acase helm-current-prefix-arg
                             ('(4)  (expand-file-name cur))
                             ('(16) (abbreviate-file-name cur))
                             (t     (helm-basename cur))))
                         (helm-marked-candidates)
                         " "))
  )
)
(defun my-helm-ff-run-copy-name ()
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'my-helm-ff-copy-name))
)
(define-key helm-find-files-map [?\C-c ?\C-w] 'my-helm-ff-run-copy-name)

(defun my-helm-ff-magit-status ()
  "Run magit-status at the path being listed by helm-find-files."
  (interactive)
  (recentf-push helm-ff-default-directory)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (_ign) (magit-status-setup-buffer helm-ff-default-directory))))
)
(define-key helm-find-files-map [?\C-c ?\C-m] 'my-helm-ff-magit-status)

;; Custom action menu for helm-find-files. There are so many default entries I
;; don't want or prefer different text labels that it's simpler to replace the
;; whole thing.
(setq helm-find-files-actions
      '(("Edit file" . helm-find-file-or-marked)
        ("Launch dired on cwd pointing at file"
         . helm-point-file-in-dired)
        ("View file" . view-file)
        ("Edit file other window `C-o'" . helm-find-files-other-window)
        ("Open file with default tool `C-c C-j'"
         . helm-open-file-with-default-tool)
        ("Launch magit on cwd `C-c C-m'"
         . (lambda (_ign)
             (magit-status-setup-buffer helm-ff-default-directory)))
        ("Bookmarks (just helm find files) `C-c C-b' (`C-c C-m' to set)"
         . (lambda (_ign) (helm-ff-bookmark)))
        ("Serial rename files" . helm-ff-serial-rename)
        ("Serial rename by symlinking files"
         . helm-ff-serial-rename-by-symlink)
        ("Serial rename by copying files" . helm-ff-serial-rename-by-copying)
        ("Search subtree paths `C-s'" . helm-ff-find-sh-command)
        ("Grep cwd with AG `M-g a' (C-u select type)" . helm-find-files-ag)
        ("Git grep cwd `M-g g'" . helm-ff-git-grep)
        ("Zgrep marked files `M-g z' (C-u recurse)" . helm-ff-zgrep)
        ("Query replace contents on marked files `M-%'"
         . helm-ff-query-replace)
        ("Query replace regexp contents on marked files `C-M-%'"
         . helm-ff-query-replace-regexp)
        ("Delete `M-D' (C-u opposite of trash setting)" . helm-ff-delete-files)
        ("Copy `M-C' (C-u to follow)" . helm-find-files-copy)
        ("Rsync marked files `M-V' (C-u edit command)" . helm-find-files-rsync)
        ("Move `M-R' (C-u to follow)" . helm-find-files-rename)
        ("Symlink `M-S' (C-u to follow)" . helm-find-files-symlink)
        ("Relsymlink `M-Y' (C-u to follow)" . helm-find-files-relsymlink)
        ("Insert selection full path into buffer `C-c i' (C-u basename)"
         . helm-insert-file-name-completion-at-point)
        ("Marked file names to kill-ring `C-c C-w' (C-u full path)"
         . my-helm-ff-copy-name)))

;;
;; Subtree path search config
;;

;; I don't like having to type 3 characters before helm starts a search of
;; subtree paths. This makes it immediate.
(helm-attrset 'requires-pattern 0 helm-source-findutils)

;; Helm doesn't provide any key mapping or action menu for toggling this
;; boolean. I create a key mapping and a hydra entry.
(defun my-helm-toggle-full-path-search ()
  "Toggle boolean helm-findutils-search-full-path"
  (interactive)
  (setq helm-findutils-search-full-path
        (if helm-findutils-search-full-path nil t))
)
(define-key helm-generic-files-map
  [?\C-c ?\C-a] 'my-helm-toggle-full-path-search)

;; Custom action menu for subtree path search. Same reason as for
;; helm-find-files.
(setq helm-type-file-actions
      '(("Edit file" . helm-find-many-files)
        ("Launch dired in file's directory" . helm-open-dired)
        ("View file" . view-file)
        ("Edit file other window" . helm-find-files-other-window)
        ("Open file with default tool `C-c C-j'"
         . helm-open-file-with-default-tool)
        ("Zgrep marked files (C-u recurse)" . helm-ff-zgrep)
        ("Delete" . helm-ff-delete-files)
        ("Copy `M-C' (C-u to follow)" . helm-find-files-copy)
        ("Move `M-R' (C-u to follow)" . helm-find-files-rename)
        ("Symlink `M-S' (C-u to follow)" . helm-find-files-symlink)
        ("Relsymlink (C-u to follow)" . helm-find-files-relsymlink)
        ("Checksum file" . helm-ff-checksum)))

;;
;; Buffer menu config
;;

;; The default for M-D in the buffer menu is to quit the menu after deleting.
;; I never want to do that. This stays in the menu. The original mapping for
;; this was C-c d, and I left it in place.
(define-key helm-buffer-map [?\M-D] 'helm-buffer-run-kill-persistent)

;; Custom action menu for helm-mini and helm-multi-files. Same reason as for
;; helm-find-files.
(setq helm-type-buffer-actions
      '(("Switch to buffer(s)" . helm-buffer-switch-buffers)
        ("Switch to selected buffer in view mode" . view-buffer)
        ("Switch to buffer(s) in other window `C-o'"
         . helm-buffer-switch-buffers-other-window)
        ("Display buffer in other window, but don't switch to it"
         . display-buffer)
        ("Kill buffer(s) `M-D'" . helm-kill-marked-buffers)
        ("Revert buffer(s) `M-G'" . helm-revert-marked-buffers)
        ("Rename buffer `M-R'" . helm-buffers-rename-buffer)
        ("Query replace `M-%'" . helm-buffer-query-replace)
        ("Query replace regexp `C-M-%'" . helm-buffer-query-replace-regexp)
        ("Grep buffer(s) `M-g s' (C-u grep all buffers)" . helm-zgrep-buffers)
        ("Launch occur on buffer(s) `C-s' (C-u also search current)"
         . helm-multi-occur-as-action)))

;;
;; Occur config
;;

;; Copied and modified original helm-occur. The symbol at point is actually
;; entered into the minibuffer. This allows more search terms to be added
;; without retyping the original.
;; Also, I removed the feature that limits the search to the selected region.
(defun my-helm-occur (&optional start-pattern)
  "Search lines matching pattern in current buffer like original occur

If optional arg start-pattern is given, pre-fill the search input with that.
Otherwise,pre-fill the input with the word at the point."
  (interactive)
  (setq helm-source-occur
        (car (helm-occur-build-sources (list (current-buffer)) "Helm occur")))
  (helm-set-local-variable 'helm-occur--buffer-list (list (current-buffer))
                           'helm-occur--buffer-tick
                           (list (buffer-chars-modified-tick (current-buffer))))
  (save-restriction
    (let ((helm-sources-using-default-as-input
           (unless (> (buffer-size) 2000000)
             helm-sources-using-default-as-input))
          (init-input (if (null start-pattern)
                          (helm-aif (thing-at-point 'symbol) (regexp-quote it))
                        start-pattern)))
      (unwind-protect
           (helm :sources 'helm-source-occur
                 :buffer "*helm occur*"
                 :history 'helm-occur-history
                 :input init-input            ; changed from :default
                 :preselect (and (memq 'helm-source-occur
                                       helm-sources-using-default-as-input)
                                 (format "^%d:" (line-number-at-pos
                                                  (point))))
                 :truncate-lines helm-occur-truncate-lines)
        (deactivate-mark t)
      )
    )
  )
)


(defun my-helm-explore-list (list-var)
  "Search list LIST-VAR with helm, and return the value selected."
  (cl-assert (listp (symbol-value list-var)) nil
             "Error: Attempt to explore variable that is not a list")
  (helm-comp-read "Next element matching (regexp): "
                  (cl-loop for i in
                           (symbol-value list-var)
                           unless (equal "" i) collect i into history
                           finally return
                           (if (consp (car history))
                               (mapcar 'prin1-to-string history)
                             history))
                  :header-name (lambda (name)
                                 (format "%s (%s)" name list-var))
                  :buffer "*helm list explorer*"
                  :must-match t
                  :multiline t
                  :keymap helm-minibuffer-history-map
                  :allow-nest nil)
)


;; https://github.com/abo-abo/hydra
(defhydra my-helm-main (:color blue)
  "
Helm:
_m_ Local and global mark rings     _o_ Helm occur on current buffer
_b_ Bookmarks                       _O_ Helm occur on org mode headings
_r_ Recent files                    _I_ Org mode headings
_j_ Buffer menu                     _i_ Imenu current buffer
_f_ Multi-menu: buffers, recent files, bookmarks, and current dir
_R_ Resume last Helm (with prefix, first select from multiple)
_a_ Toggle searching in paths as well as files names (%`helm-findutils-search-full-path)

_w_ Emacs registers                 _x_ Regexp builder/tester
_c_ Colors                          _M_ Manual pages (using WOman)
_C_ Complete Emacs Lisp symbol      _X_ Helm docs
_p_ Emacs packages                  _Y_ GNU Info
_P_ Emacs processes                 _Z_ GNU Info for Emacs topics only
"
  ("C" helm-lisp-completion-at-point nil)
  ("I" helm-org-in-buffer-headings nil)
  ("M" helm-man-woman nil)
  ("O" (my-helm-occur "^\\*\\** ") nil)
  ("P" helm-list-emacs-process nil)
  ("R" helm-resume nil)
  ("X" helm-documentation nil)
  ("Y" helm-info-at-point nil)
  ("Z" helm-info-emacs nil)
  ("a" my-helm-toggle-full-path-search nil :color red)
  ("b" helm-filtered-bookmarks nil)
  ("c" helm-colors nil)
  ("f" helm-multi-files nil)
  ("i" helm-semantic-or-imenu nil)
  ("j" helm-mini nil)
  ("m" helm-all-mark-rings nil)
  ("o" my-helm-occur nil)
  ("p" helm-list-elisp-packages nil)
  ("r" helm-recentf nil)
  ("w" helm-register nil)
  ("x" helm-regexp nil)
)

(defun my-buf-menu-wrapper (arg)
  "Entry point for buffer menu.
Without a prefix, run bs-show.
With one universal prefix, run helm-multi-files
With two universal prefixes, run helm-mini."
  (interactive "P")
  (cond
   ((= 4 (prefix-numeric-value arg))
    (helm-multi-files))
   ((= 16 (prefix-numeric-value arg))
    (helm-mini))
   (t
    (call-interactively 'bs-show))
  )
)

(global-set-key [?\M-j]       'my-buf-menu-wrapper)
(global-set-key [?\C-`]       'my-buf-menu-wrapper)

(global-set-key [?\M-i]       'helm-find-files)
(global-set-key [?\C-x ?\C-f] 'helm-find-files)
(global-set-key [?\C-x ?\M-f] 'helm-find-files)
(global-set-key [?\M-y]       'helm-show-kill-ring)  ; replaces yank-pop
(global-set-key [?\C-h ?a]    'helm-apropos)
;; I never use the default prefix (C-x c). Instead, I use a hydra with a
;; subset of the helm functions available through the prefix key.
(global-set-key [?\M-g ?\M-h] 'my-helm-main/body)

;; Using M-x as an alias for C-x enables faster typing of C-x sequences that
;; don't involve the Ctrl key.  Moving the traditional M-x to double M-x is an
;; acceptable inconvenience to me.
(global-set-key [?\M-x]       'Control-X-prefix)
(global-set-key [?\C-x ?\M-x] 'helm-M-x)
