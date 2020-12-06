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
;; Startup and global stuff
;;
;; Define a boolean I will need throughout my startup code.
(setq my-unix-p (let ((ostype (getenv "OS")))
                  (if (and ostype (string-match "Windows" ostype)) nil t)))

(let ((loc (expand-file-name "~/.emacs.local.el")))
  (if (file-exists-p loc) (load loc))
)

;; gnuserv is like emacsclient for Windows.  It doesn't work with Emacs 23.
;; Under Linux, the variable gnuserv-frame is ignored.  I'm pretty sure this
;; needs to occur before server-start.
(and (not my-unix-p) (< emacs-major-version 23) (load "gnuserv" t))
(setq gnuserv-frame (selected-frame))

;; This collects all command-line calls to emacsclient in one running process.
;; Use C-x # (which is the same as M-x server-edit) when done editing a file
;; launched in this fashion.  Emacs 23 (additionally) supports a very
;; different way of doing it, and this will return an error if you run emacs
;; (instead of emacsclient) while emacs --daemon is already running.
(server-start)

;; Don't show the start-up/splash screen, and don't insert explanatory text
;; into the initial scratch buffer.  In Emacs 22 and earlier, the second
;; setting isn't necessary.
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Override a built-in function definition with another to make all "yes or
;; no" prompts show "y or n" instead.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable obnoxious ~/.saves-foo-bar files.
(setq auto-save-list-file-prefix nil)

;; Disable obnoxious backup files.
(setq make-backup-files nil)
;; For CVS, too.
(setq-default vc-cvs-stay-local nil)

;; Disable auto-save completely. It has never rescued me, and I manually save
;; religiously, anyway.
(setq auto-save-default nil)

;; Disable obnoxious prompt when following symlinks to RCS or CVS files.
(setq vc-follow-symlinks nil)

;; Enable the processing of local variables in files visited.  We're setting
;; it to the default value of t, so the purpose here is really to explain with
;; this comment.  The value nil disables it, and any other value causes the
;; user to be prompted when visiting the file but before displaying it.  You
;; can always interactively run M-x normal-mode to force processing local
;; variables in the current file.
(setq enable-local-variables t)

;; The special local variable "eval" inside a file can be dangerous, and so it
;; has its own enable flag.  The meaning is interpreted the same as
;; enable-local-variables.  The default is to prompt, which I consider to be
;; obnoxious.  Since I would probably never use this feature in my own files,
;; I choose to disable the dangerous stuff.  Note that the help text says this
;; applies only to eval, but the Emacs manual says it also applies to variable
;; names that end in -hook, -hooks, -function, or -functions.
(setq enable-local-eval nil)

;; Tell Emacs that in my OS, the Window focus does not follow the mouse.
(setq focus-follows-mouse nil)


;;
;; The rest of startup is broken down into files by area of functionality.
;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp") t)

;; Load all installed packages first
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(load "load-save-exit")
(load "basic-move-edit")
(load "kill-yank")
(load "buffer-select")
(load "search-replace")
(load "grep-compile")
(load "my-indent")
(load "my-shell")
(load "my-dired")
(load "my-completion")
(load "my-vcs")
(load "my-dirtree")
(load "my-org")
(load "my-helm")
(load "my-misc")
(load "my-html")

;; This is near the end because it's very disruptive visually, and
;; color-theme.el is huge and slow.  Also, if there are any errors, I want
;; them to occur after as many of my preferred settings are configured as
;; possible.
(load "appearance")

;; At one time, I needed this to load last because of quirky interactions
;; between protobuf-mode and cc-mode. That no longer appears to be true, but
;; it doesn't hurt for this to be last.
(load "prg-modes")

;; Generated automatically by installing a package, M-x customize-variable,
;; and others. Use git to check for actual, desirable changes afterward.
;; Beware: Must have only one instance of this function call in the init
;; files.
(custom-set-variables
 ;; Restore behavior of older Emacs version when navigating history at the
 ;; query-replace minibuffer prompt: search and replace strings get separate
 ;; entries.
 ;; Setting this with ordinary setq causes the default value to re-appear
 ;; after each call to a replace function. I don't understand why.
 '(query-replace-from-to-separator nil)

 ;; Installed packages
 '(package-selected-packages
   '(ag async dired-efap exec-path-from-shell go-mode groovy-mode helm
     htmlize hydra magit org powershell protobuf-mode rainbow-mode tree-mode
     yaml-mode yasnippet))
)
