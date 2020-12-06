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
(defun my-del-from-alist-by-cdr (lst victim)
  "Delete an element from alist 'lst'
by looking for the cdr of that matches 'victim'."
  (delete-if (lambda (el) (eq (cdr el) victim))
             (symbol-value lst))
)

(require 'helm)
(require 'helm-config)
(require 'helm-find)
(require 'helm-bookmark)
(setq bookmark-save-flag 1)
(setq helm-buffers-fuzzy-matching t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-ff-file-name-history-use-recentf t)

;; This function is hard to describe. It basically means do the intuitive
;; thing. For some reason, the default for this is C-z.
(define-key helm-map [tab]   'helm-execute-persistent-action)
(define-key helm-map [?\C-i] 'helm-execute-persistent-action)
;; Most helm sessions have a choice of different actions you can perform on
;; your selected files. This toggles to a menu showing those action choices.
(define-key helm-map [backtab] 'helm-select-action)

(helm-mode 1)

;; Prevents helm from activating on any of these actions.
(cl-loop for trigger in '((dired-do-rename)
                          (dired-do-copy)
                          (dired-do-symlink)
                          (dired-do-relsymlink)
                          (dired-do-hardlink)
                          (write-file)
                          (basic-save-buffer))
         do (add-to-list 'helm-completing-read-handlers-alist trigger))

;; Action menu entries I never use. Declutter those menus.
(let ((blacklist '(find-alternate-file
                   find-file-other-frame
                   helm-buffers-browse-project
                   helm-ediff-marked-buffers
                   helm-ff-browse-project
                   helm-ff-cache-add-file
                   helm-ff-etags-select
                   helm-ff-fd
                   helm-ff-gid
                   helm-ff-locate
                   helm-ff-mail-attach-files
                   helm-ff-pdfgrep
                   helm-ff-print
                   helm-ff-query-replace-fnames-on-marked
                   helm-ff-switch-to-shell
                   helm-ff-touch-files
                   helm-files-insert-as-org-link
                   helm-find-file-as-root
                   helm-find-files-backup
                   helm-find-files-ediff-files
                   helm-find-files-ediff-merge-files
                   helm-find-files-eshell-command-on-file
                   helm-find-files-grep
                   helm-find-files-hardlink
                   helm-marked-files-in-dired
                   helm-open-file-externally
                   hexl-find-file
                   switch-to-buffer-other-frame
                   switch-to-buffer-other-tab)))
  (cl-loop for action-menu in '(helm-find-files-actions
                                helm-type-file-actions
                                helm-type-buffer-actions)
           do (mapcar (lambda (victim)
                        (my-del-from-alist-by-cdr action-menu victim))
                      blacklist)
  )
)
;; These are replaced below
(mapcar
 (lambda (victim) (my-del-from-alist-by-cdr 'helm-find-files-actions victim))
 '(helm-ff-find-sh-command
   helm-find-files-other-window
   helm-open-file-with-default-tool))
(my-del-from-alist-by-cdr 'helm-type-file-actions
                          'helm-open-file-with-default-tool)

;; To search paths below the current location, C-s is much more intuitive
;; than the default "C-c /". The original function on C-s is still
;; available on "M-g s".
(add-to-list
 'helm-find-files-actions
 '("Search subtree paths `C-s'" . helm-ff-find-sh-command) t)
(define-key helm-find-files-map [?\C-s] 'helm-ff-run-find-sh-command)

;; To open the selection in the other window, C-o is much easier to type
;; than the default "C-c o".
(add-to-list
 'helm-find-files-actions
 '("Open file other window `C-o'" . helm-find-files-other-window) t)
(define-key helm-find-files-map    [?\C-o] 'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map [?\C-o] 'helm-ff-run-switch-other-window)
(define-key helm-buffer-map        [?\C-o] 'helm-buffer-switch-other-window)
(define-key helm-bookmark-map      [?\C-o] 'helm-bookmark-run-jump-other-window)

;; The default for M-D in the buffer menu is to quit the menu after deleting.
;; I never want to do that. This stays in the menu. The original mapping for
;; this was C-c d, and I left it also on that key.
(define-key helm-buffer-map [?\M-D] 'helm-buffer-run-kill-persistent)

;; The default mapping for C-/ is helm-ff-run-fd, but I don't use the fd
;; command.
(define-key helm-find-files-map [?\C-/] 'helm-ff-undo)

;; The default mapping is C-c X. I haven't overwritten that.
(add-to-list
 'helm-find-files-actions
 '("Open file with default tool `C-c C-j'" . helm-open-file-with-default-tool)
 t)
(add-to-list
 'helm-type-file-actions
 '("Open file with default tool `C-c C-j'" . helm-open-file-with-default-tool)
 t)
(define-key helm-find-files-map    [?\C-c ?\C-j]
  'helm-ff-run-open-file-with-default-tool)
(define-key helm-generic-files-map [?\C-c ?\C-j]
  'helm-ff-run-open-file-with-default-tool)

;; The default mapping is C-x r b. I haven't overwritten that. It doesn't
;; appear in the action menu by default.
(add-to-list
 'helm-find-files-actions
 '("Bookmarks (just helm find files) `C-c C-b' (`C-c C-m' to set)"
   . (lambda (x) (helm-ff-bookmark))) t)
(define-key helm-find-files-map [?\C-c ?\C-b]
  'helm-find-files-switch-to-bookmark)

;; The default mapping is C-x r m. I haven't overwritten that. It doesn't
;; appear in the action menu by default, and I haven't figured out a good way
;; to add it. So, I put a keyboard hint in the bookmark action created above.
(define-key helm-find-files-map [?\C-c ?\C-m] 'helm-ff-bookmark-set)

;; The default mapping is C-c h. I haven't overwritten that.
;; The default for M-r is rotate a viewed image file right. (M-l rotates
;; left.) I don't have that rotation feature working, and I'd never use it.
(define-key helm-find-files-map [?\M-r] 'helm-ff-file-name-history)

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
(add-to-list
 'helm-find-files-actions
 '("Marked file names to kill ring `C-c C-w' (C-u full path)"
   . my-helm-ff-copy-name)
 t)

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

;; Copied and modified original helm-occur. The symbol at point is actually
;; entered into the minibuffer. This allows more search terms to be added
;; without retyping the original.
;; Also, I removed the feature that limits the search to the selected region.
(defun my-helm-occur ()
  "Search lines matching pattern in current buffer like original occur."
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
          def pos)
      (unwind-protect
           (helm :sources 'helm-source-occur
                 :buffer "*helm occur*"
                 :history 'helm-occur-history
                 ;; Changed from :default
                 :input (or def (helm-aif (thing-at-point 'symbol)
                                    (regexp-quote it)))
                 :preselect (and (memq 'helm-source-occur
                                       helm-sources-using-default-as-input)
                                 (format "^%d:" (line-number-at-pos
                                                 (or pos (point)))))
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
_m_ Local and global mark rings     _o_ Search current buffer with helm-occur
_b_ Bookmarks                       _x_ Regexp builder/tester
_r_ Recent files                    _i_ Imenu current buffer
_j_ Buffer menu                     _I_ Imenu all buffers
_f_ Multi-menu: buffers, recent files, bookmarks, and current dir
_R_ Resume last Helm (with prefix, first select from multiple)
_a_ Toggle searching in paths as well as files names (%`helm-findutils-search-full-path)

_w_ Emacs registers                 _M_ Manual pages (using WOman)
_c_ Colors                          _X_ Helm docs
_C_ Complete Emacs Lisp symbol      _Y_ GNU Info
_p_ Emacs packages                  _Z_ GNU Info for Emacs topics only
_P_ Emacs processes
"
  ("C" helm-lisp-completion-at-point nil)
  ("I" helm-imenu-in-all-buffers nil)
  ("M" helm-man-woman nil)
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
