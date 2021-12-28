;;; uwu-theme.el --- An awesome dark color scheme  -*- lexical-binding: t; -*-

;; Author: Kevin Borling <https://github.com/kborling>
;; Created: December 24, 2021
;; Version: 1.0.0
;; Keywords: custom themes, faces
;; URL: https://github.com/kborling/uwu.el
;; Homepage: https://github.com/kborling/uwu.el
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; Copyright (C) 2021 Kevin Borling
;; Inspired by uwu theme for vim <https://github.com/Mangeshrex/uwu.vim>

;;; Code:

(deftheme uwu "UwU color theme")

;;; Variables
(defvar uwu-colors-alist
  '(("uwu-fg"                . "#C5C8C9")
    ("uwu-bg"                . "#131A1C")
    ("uwu-highlight"         . "#2F3638")
    ("uwu-comment"           . "#62686A")
    ("uwu-black"             . "#1B2224")
    ("uwu-red"               . "#F65B5B")
    ("uwu-green"             . "#6BB05D")
    ("uwu-yellow"            . "#E59E67")
    ("uwu-blue"              . "#53A7BF")
    ("uwu-magenta"           . "#B185DB")
    ("uwu-cyan"              . "#51A39F")
    ("uwu-white"             . "#C4C4C4")
    ("uwu-bright-black"      . "#232A2C")
    ("uwu-bright-red"        . "#C26F6F")
    ("uwu-bright-green"      . "#8DC776")
    ("uwu-bright-yellow"     . "#E7AC7E")
    ("uwu-bright-blue"       . "#6CBAD1")
    ("uwu-bright-magenta"    . "#BB8FE5")
    ("uwu-bright-cyan"       . "#6DB0AD")
    ("uwu-bright-white"      . "#C5C8C9")
    ("uwu-warning"           . "red")))

(defmacro uwu-with-color-variables (&rest body)
  "`let' bind all colors defined in `uwu-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append uwu-colors-alist)))
     ,@body))

;;; Theme Faces
(uwu-with-color-variables
  (custom-theme-set-faces 'uwu
                          '(button ((t (:underline t))))
                          `(font-lock-keyword-face ((t (:foreground ,uwu-magenta))))
                          `(font-lock-function-name-face ((t (:foreground ,uwu-blue))))
                          `(font-lock-string-face ((t (:foreground ,uwu-green))))
                          `(font-lock-warning-face ((t (:inverse-video t :background ,uwu-white :foreground ,uwu-red))))
                          `(underline ((t (:underline t :foreground ,uwu-yellow))))
                          `(font-lock-type-face ((t (:weight bold :foreground ,uwu-yellow))))
                          `(font-lock-preprocessor-face ((t (:foreground ,uwu-blue))))
                          `(font-lock-builtin-face ((t (:weight bold :foreground ,uwu-yellow))))
                          `(font-lock-variable-name-face ((t (:foreground ,uwu-red))))
                          `(font-lock-constant-face ((t (:foreground ,uwu-yellow))))
                          `(font-lock-doc-face ((t (:slant italic :foreground ,uwu-comment))))
                          `(font-lock-comment-face ((t (:slant italic :foreground ,uwu-comment))))
                          `(show-paren-match-face ((t (:inverse-video t :background ,uwu-white :foreground ,uwu-red))))
                          `(default ((t (:background ,uwu-bg :foreground ,uwu-fg))))
                          `(cursor ((t (:background ,uwu-white :foreground ,uwu-bright-black))))
                          `(highline-face ((t (:background ,uwu-black))))
                          `(ac-selection-face ((t (:background ,uwu-magenta :foreground ,uwu-highlight))))
                          `(ac-candidate-face ((t (:background ,uwu-black :foreground ,uwu-white))))
                          `(flyspell-duplicate ((t (:underline t :style wave :foreground ,uwu-magenta))))
                          `(flyspell-incorrect ((t (:underline t :style wave :foreground ,uwu-red))))
                          `(link ((t (:underline t :foreground ,uwu-blue))))
                          `(minibuffer-prompt ((t (:foreground ,uwu-yellow))))
                          `(highlight ((t (:background ,uwu-highlight))))
                          `(region ((t (:inverse-video t :background ,uwu-black))))
                          `(mode-line-inactive ((t (:background ,uwu-black :foreground ,uwu-comment))))
                          `(mode-line-buffer-id ((t (:weight bold :background ,uwu-black :foreground ,uwu-bright-white))))
                          `(mode-line ((t (:weight bold :background ,uwu-black :foreground ,uwu-bright-white))))
                          `(fringe ((t (:underline t :foreground ,uwu-highlight))))
                          `(linum ((t (:underline t :foreground ,uwu-highlight))))
                          `(isearch ((t (:inverse-video t :background ,uwu-highlight :foreground ,uwu-blue))))
                          `(isearch-lazy-highlight-face ((t (:inverse-video t :foreground ,uwu-yellow))))
                          `(dired-directory ((t (:weight bold :foreground ,uwu-blue))))))



;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'uwu)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; uwu-theme.el ends here
