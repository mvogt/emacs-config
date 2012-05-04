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
;; Minibuffer completion
;;
;; In the pop-up completions buffer, show case-insensitive matches.
(setq read-file-name-completion-ignore-case t)
;; ...and don't print boilerplate text at the top.
(setq completion-show-help nil)

;; Makes default completion tolerable until I learn ido or icicles.
(defun my-toggle-completions-window ()
  "Toggle between the completions window and the minibuffer.
When switching back to the minibuffer, quit the completions window to show the
buffer that was there before."
  (interactive)
  (if (not (eq (get-buffer-window) (get-buffer-window "*Completions*")))
      (switch-to-completions)
    (quit-window)
    (select-window (minibuffer-window))
  )
)

(define-key minibuffer-local-map     [backtab] 'my-toggle-completions-window)
(define-key completion-list-mode-map [backtab] 'my-toggle-completions-window)
(define-key completion-list-mode-map [?q]      'my-toggle-completions-window)
(define-key completion-list-mode-map [?\C-g]   'my-toggle-completions-window)


;; Disable Tramp's completion function for SSH-related protocols. It parses
;; host names from ~/.ssh/config and ~/.ssh/known_hosts, and it fails loudly
;; when they're unreachable -- even when the name is only partially spelled
;; out. This is obnoxious when there are SSH aliases or host names with
;; spellings similar to frequently visited local paths.
(require 'tramp)
(tramp-set-completion-function "scp" nil)
(tramp-set-completion-function "ssh" nil)
