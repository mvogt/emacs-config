;;
;; Startup and global stuff
;;

;; Define a boolean I will need throughout my startup code.
(setq my-unix-p (let ((ostype (getenv "OS")))
                  (if (and ostype (string-match "Windows" ostype)) nil t)))

;; For appending subdirs to my startup dir path.
(setq my-startup-file-path (and load-file-name
                                (file-name-directory load-file-name)))

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


;;
;; The rest of startup is broken down into files by area of functionality.
;;

(load "load-save-exit")
(load "basic-move-edit")
(load "kill-yank")
(load "buffer-select")
(load "search-replace")
(load "grep-compile")
(load "my-indent")
(load "prg-modes")
(load "my-shell")
(load "my-dired")
(load "my-completion")
(load "my-misc")
(load "my-html")

;; Separate package by the author of YASnippet, but they go well together.
;; http://code.google.com/p/autopair/
;; http://www.emacswiki.org/emacs/AutoPairs
(require 'autopair)
(autopair-global-mode 1)

;; This is at the end because it's very disruptive visually, and
;; color-theme.el is huge and slow.  Also, if there are any errors, I want
;; them to occur after as many of my preferred settings are configured as
;; possible.
(load "appearance")
