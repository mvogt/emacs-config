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
;; Customizations to the standard color-theme.el found at
;; http://www.emacswiki.org/cgi-bin/wiki/ColorTheme
;;
;; To browse themes, run M-x color-theme-select.  Press enter on a theme
;; to activate it.  Press ? on a theme to see its function name and help
;; text.
;;
;; Note: Before I discovered color themes, my preferred background color was
;; #F3F1DE.
;;
(require 'color-theme)

(defun color-theme-ramangalahy-improved ()
  "Tweaks to theme 'Ramangalahy' from emacs-goodies-el."
  (interactive)
  (color-theme-install
   '(color-theme-ramangalahy-improved
     ((background-color . "lightgrey")
      (background-mode . light)
      (background-toolbar-color . "#bfbfbfbfbfbf")
      (border-color . "#000000000000")
      (bottom-toolbar-shadow-color . "#737373737373")
      (cursor-color . "blue")
      (foreground-color . "black")
      (top-toolbar-shadow-color . "#e6e6e6e6e6e6"))
     ((gnus-mouse-face . highlight)
      (goto-address-mail-face . info-xref)
      (ispell-highlight-face . highlight)
      (notes-bold-face . notes-bold-face)
      (setnu-line-number-face . bold)
      (tinyreplace-:face . highlight)
      (vm-highlight-url-face . bold-italic)
      (vm-highlighted-header-face . bold)
      (vm-mime-button-face . gui-button-face)
      (vm-summary-highlight-face . bold))
     (default ((t (nil))))
     (bbdb-company ((t (nil))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (border-glyph ((t (nil))))
     (sh-heredoc ((t (:foreground "blue"))))
     (cperl-here-face ((t (:foreground "green4"))))
     (cperl-pod-face ((t (:foreground "brown4"))))
     (cperl-pod-head-face ((t (:foreground "steelblue"))))
     (cperl-array-face ((t (:foreground "magenta3"))))
     (cperl-hash-face ((t (:foreground "magenta3"))))
     (cperl-nonoverridable-face ((t (:bold t :foreground "violetred"))))
     (custom-button-face ((t (:bold t))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:underline t :foreground "blue"))))
     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-button-face ((t (:underline t :bold t))))
     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))
     (dired-header ((t (:bold t :foreground "steelblue"))))
     (dired-directory ((t (:foreground "blue"))))
     (dired-executable ((t (:foreground "chocolate4"))))
     (dired-symlink ((t (:foreground "green4"))))
     (dired-perm-write ((t (:background "grey75" :foreground "black"))))
     (dired-ignored ((t (nil))))
     (dired-marked ((t (:background "wheat2"))))
     (dired-flagged ((t (:background "wheat3"))))
     (dired-efap-face ((t (:background "darkseagreen2"))))
     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))
     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))
     (display-time-time-balloon-face ((t (:foreground "red"))))
     (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))
     (font-lock-comment-face ((t (:foreground "slate gray"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "slateblue"))))
     (font-lock-emphasized-face ((t (:bold t :background "lightyellow2"))))
     (font-lock-function-name-face ((t (:bold t :foreground "blue"))))
     (font-lock-keyword-face ((t (:bold t :foreground "violetred"))))
     (font-lock-other-emphasized-face ((t (:italic t :bold t :background "lightyellow2"))))
     (font-lock-other-type-face ((t (:bold t :foreground "orange3"))))
     (font-lock-preprocessor-face ((t (:bold t :foreground "mediumblue"))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "green4"))))
     (font-lock-type-face ((t (:bold t :foreground "steelblue"))))
     (font-lock-variable-name-face ((t (:foreground "magenta4"))))
     (font-lock-warning-face ((t (:bold t :background "yellow" :foreground "Red"))))
     (fringe ((t (:background "lightgrey"))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:background "lightgrey"))))
     (highlight ((t (:background "darkseagreen2"))))
     (info-node ((t (:underline t :bold t :foreground "mediumpurple"))))
     (info-xref ((t (:underline t :bold t :foreground "#0000ee"))))
     (isearch ((t (:background "paleturquoise"))))
     (italic ((t (:italic t))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:background "gray68" :foreground "black"))))
     (message-cited-text ((t (:foreground "slategrey"))))
     (message-cited-text-face ((t (:foreground "red"))))
     (message-header-cc-face ((t (:foreground "MidnightBlue"))))
     (message-header-contents ((t (:italic t))))
     (message-header-name-face ((t (:foreground "cornflower blue"))))
     (message-header-newsgroups-face ((t (:bold t :foreground "blue4"))))
     (message-header-other-face ((t (:foreground "steel blue"))))
     (message-header-subject-face ((t (:bold t :foreground "navy blue"))))
     (message-header-to-face ((t (:bold t :foreground "MidnightBlue"))))
     (message-header-xheader-face ((t (:foreground "blue"))))
     (message-headers ((t (:bold t))))
     (message-highlighted-header-contents ((t (:bold t))))
     (message-separator-face ((t (:foreground "brown"))))
     (message-url ((t (:bold t))))
     (modeline ((t (:bold t :background "Gray75" :foreground "Black"))))
     (modeline-buffer-id ((t (:bold t :background "Gray75" :foreground "blue4"))))
     (modeline-mousable ((t (:bold t :background "Gray75" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:bold t :background "Gray75" :foreground "green4"))))
     (paren-blink-off ((t (:foreground "lightgrey"))))
     (paren-match ((t (:background "darkseagreen2"))))
     (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))
     (pointer ((t (:foreground "blue"))))
     (primary-selection ((t (:background "gray65"))))
     (red ((t (:foreground "red"))))
     (region ((t (:background "paleturquoise"))))
     (right-margin ((t (nil))))
     (searchm-buffer ((t (:bold t :background "white" :foreground "red"))))
     (searchm-button ((t (:bold t :background "CadetBlue" :foreground "white"))))
     (searchm-field ((t (:background "grey89"))))
     (searchm-field-label ((t (:bold t))))
     (searchm-highlight ((t (:bold t :background "darkseagreen2" :foreground "black"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (template-message-face ((t (:bold t))))
     (text-cursor ((t (:background "blue" :foreground "lightgrey"))))
     (toolbar ((t (nil))))
     (underline ((t (:underline t))))
     (vertical-divider ((t (nil))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (x-face ((t (:background "white" :foreground "black"))))
     (x-symbol-adobe-fontspecific-face ((t (nil))))
     (x-symbol-face ((t (nil))))
     (x-symbol-heading-face ((t (:underline t :bold t :foreground "green4"))))
     (x-symbol-info-face ((t (:foreground "green4"))))
     (x-symbol-invisible-face ((t (nil))))
     (x-symbol-revealed-face ((t (:background "pink"))))
     (yellow ((t (:foreground "yellow"))))
     (zmacs-region ((t (:background "yellow"))))
     (compilation-info ((t (:foreground "green4"))))
     (diff-context ((t (:foreground "slate gray"))))
     (diff-removed ((t (:foreground "green4"))))
     (diff-added ((t (:foreground "blue"))))
     (diff-refine-change ((t (:background "paleturquoise")))))))

(add-to-list 'color-themes '(color-theme-ramangalahy-improved
                             "Tweaked 'Ramangalahy'" "local user"))

(color-theme-ramangalahy-improved)
