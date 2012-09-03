;;
;; Copyright 2012  Mark Vogt
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
;; Directory tree mode
;;
(require 'dirtree)

(defun my-dirtree-find-child (tree file)
  "Given a dirtree tree object, select and expand the child matching file."
  (catch 'break
    (dolist (cur (widget-get tree :children))
      (when (string= file (widget-get (widget-get cur :node) :file))
        (unless (widget-get cur :open)
          (widget-apply-action cur)
        )
        (goto-char (widget-get cur :from))
        (throw 'break cur)
      )
    )
  )
)

(defun my-dirtree-find-full-path (full-path)
  "Select the given path in the current dirtree.
Expands parent nodes as needed from the root to make it visible.
Assumes there's only one dirtree buffer, and it's rooted at /.
Does nothing if the current buffer is not in dirtree-mode."
  (interactive "fPath: ")
  (let ((remaining (split-string full-path "/" t))
        (cur-path "")
        (tree (car tree-mode-list)))
    (with-current-buffer dirtree-buffer
      (dolist (path-elem remaining)
        (setq cur-path (concat cur-path "/" path-elem))
        (setq tree (my-dirtree-find-child tree cur-path))
      )
    )
  )
)

(defun my-dirtree-rootfs-cwd (other-p)
  "Switch to the dirtree, and select the current buffer's directory in it.
With prefix argument, create in other window.
Assumes there's only one dirtree buffer, and it's rooted at /."
  (interactive "P")
  (let ((dest-path (expand-file-name default-directory)))
    (dirtree "/" other-p)
    (my-dirtree-find-full-path dest-path)
  )
)

(defun my-dirtree-rootfs (other-p)
  "Create tree of directory '/' and select it.
With prefix argument, create in other window."
  (interactive "P")
  (dirtree "/" other-p)
)

(defun my-tree-mode-collapse-level ()
  "Collapse tree on current line."
  (interactive)
  (let ((me (tree-mode-icon-current-line)))
    (unless (tree-widget-leaf-node-icon-p me)
      (goto-char (widget-get me :from))
      (setq me (widget-get me :parent))
      (if (widget-get me :open)
          (widget-apply-action me)
      )
    )
  )
)

(defun my-bs-dirtree-wrapper (arg)
  "Launch bs-show without a prefix or dirtree with one.
Intended to combine them into one key binding."
  (interactive "P")
  (if arg (my-dirtree-rootfs-cwd t) (call-interactively 'bs-show))
)
(global-set-key [?\M-j] 'my-bs-dirtree-wrapper)
(global-set-key [?\C-`] 'my-bs-dirtree-wrapper)
(global-set-key [?\C-~] 'my-dirtree-rootfs-cwd)

(define-key dirtree-mode-map [up]       'tree-mode-previous-node)
(define-key dirtree-mode-map [?\C-p]    'tree-mode-previous-node)
(define-key dirtree-mode-map [down]     'tree-mode-next-node)
(define-key dirtree-mode-map [?\C-n]    'tree-mode-next-node)
(define-key dirtree-mode-map [right]    'tree-mode-expand-level)
(define-key dirtree-mode-map [?\C-f]    'tree-mode-expand-level)
(define-key dirtree-mode-map "+"        'tree-mode-expand-level)
(define-key dirtree-mode-map "="        'tree-mode-expand-level)
(define-key dirtree-mode-map [left]     'tree-mode-goto-parent)
(define-key dirtree-mode-map [?\C-b]    'tree-mode-goto-parent)
(define-key dirtree-mode-map "-"        'my-tree-mode-collapse-level)
(define-key dirtree-mode-map "*"        (lambda () (interactive)
                                          (tree-mode-expand-level 0)))
(define-key dirtree-mode-map "\\"       (lambda () (interactive)
                                          (tree-mode-expand-level 1)))
(define-key dirtree-mode-map "|"        'tree-mode-collapse-other-except)
(define-key dirtree-mode-map "\t"       'tree-mode-toggle-expand)
(define-key widget-keymap "\t"          'tree-mode-toggle-expand)
(define-key widget-keymap "\e\t"        'tree-mode-toggle-expand)
(define-key widget-keymap [(shift tab)] 'tree-mode-toggle-expand)
(define-key widget-keymap [backtab]     'tree-mode-toggle-expand)

(defun my-dirtree-display-other-window ()
  (interactive)
  (dirtree-display t)
)
(define-key dirtree-mode-map "o"        'my-dirtree-display-other-window)
(define-key dirtree-mode-map [C-return] 'my-dirtree-display-other-window)
(define-key dirtree-mode-map [?\C-j]    'my-dirtree-display-other-window)
