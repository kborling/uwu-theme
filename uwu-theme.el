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

(deftheme uwu "UwU color theme.")

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

(defvar uwu-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar uwu-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar uwu-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom uwu-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'uwu-theme
  :package-version '(uwu . "1.0"))

(defcustom uwu-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'uwu-theme
  :package-version '(uwu . "1.0"))

(defcustom uwu-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'uwu-theme
  :package-version '(uwu . "1.0"))

(defcustom uwu-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'uwu-theme
  :package-version '(uwu . "1.0"))

(defcustom uwu-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'uwu-theme
  :package-version '(uwu . "1.0"))


(defmacro uwu-with-color-variables (&rest body)
  "`let' bind all colors defined in `uwu-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append uwu-colors-alist))
         (z-variable-pitch (if uwu-use-variable-pitch
                               'variable-pitch 'default)))
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
                          `(linum ((t (:foreground ,uwu-highlight :background ,uwu-bg))))
                          ;;;;; hl-line-mode
                          `(hl-line-face ((,class (:background ,uwu-highlight))
                                          (t :weight bold)))
                          `(hl-line ((,class (:background ,uwu-highlight :extend t)) ; old emacsen
                                     (t :weight bold)))
                          ;;;;; hl-sexp
                          `(hl-sexp-face ((,class (:background ,uwu-highlight))
                                          (t :weight bold)))
                          ;;;;; isearch
                          `(isearch ((t (:inverse-video t :background ,uwu-highlight :foreground ,uwu-blue))))
                          `(isearch-lazy-highlight-face ((t (:inverse-video t :foreground ,uwu-yellow))))
                          `(dired-directory ((t (:weight bold :foreground ,uwu-blue))))
                          ;;;;; org-mode
                          `(org-agenda-date-today
                            ((t (:foreground ,uwu-fg :slant italic :weight bold))) t)
                          `(org-agenda-structure
                            ((t (:inherit font-lock-comment-face))))
                          `(org-archived ((t (:foreground ,uwu-fg :weight bold))))
                          `(org-block ((t (:background ,uwu-bg :extend t))))
                          `(org-checkbox ((t (:background ,uwu-bg :foreground ,uwu-fg
                                                          :box (:line-width 1 :style released-button)))))
                          `(org-date ((t (:foreground ,uwu-blue :underline t))))
                          `(org-deadline-announce ((t (:foreground ,uwu-red))))
                          `(org-done ((t (:weight bold :weight bold :foreground ,uwu-green))))
                          `(org-formula ((t (:foreground ,uwu-yellow))))
                          `(org-headline-done ((t (:foreground ,uwu-green))))
                          `(org-hide ((t (:foreground ,uwu-bg))))
                          `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,uwu-blue
                                                      ,@(when uwu-scale-org-headlines
                                                          (list :height uwu-height-plus-4))))))
                          `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,uwu-green
                                                      ,@(when uwu-scale-org-headlines
                                                          (list :height uwu-height-plus-3))))))
                          `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,uwu-magenta
                                                      ,@(when uwu-scale-org-headlines
                                                          (list :height uwu-height-plus-2))))))
                          `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,uwu-red
                                                      ,@(when uwu-scale-org-headlines
                                                          (list :height uwu-height-plus-1))))))
                          `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,uwu-bright-red))))
                          `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,uwu-bright-magenta))))
                          `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,uwu-bright-green))))
                          `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,uwu-bright-blue))))
                          `(org-link ((t (:foreground ,uwu-yellow :underline t))))
                          `(org-scheduled ((t (:foreground ,uwu-green))))
                          `(org-scheduled-previously ((t (:foreground ,uwu-red))))
                          `(org-scheduled-today ((t (:foreground ,uwu-blue))))
                          `(org-sexp-date ((t (:foreground ,uwu-blue :underline t))))
                          `(org-special-keyword ((t (:inherit font-lock-comment-face))))
                          `(org-table ((t (:foreground ,uwu-blue))))
                          `(org-tag ((t (:weight bold :weight bold))))
                          `(org-time-grid ((t (:foreground ,uwu-yellow))))
                          `(org-todo ((t (:weight bold :foreground ,uwu-red :weight bold))))
                          `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
                          `(org-warning ((t (:weight bold :foreground ,uwu-red :weight bold :underline nil))))
                          `(org-column ((t (:background ,uwu-bg))))
                          `(org-column-title ((t (:background ,uwu-bg :underline t :weight bold))))
                          `(org-mode-line-clock ((t (:foreground ,uwu-fg :background ,uwu-bg))))
                          `(org-mode-line-clock-overrun ((t (:foreground ,uwu-bg :background ,uwu-red))))
                          `(org-ellipsis ((t (:foreground ,uwu-yellow :underline t))))
                          `(org-footnote ((t (:foreground ,uwu-cyan :underline t))))
                          `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,uwu-blue
                                                             :weight bold
                                                             ,@(when uwu-scale-org-headlines
                                                                 (list :height uwu-height-plus-4))))))
                          `(org-document-info ((t (:foreground ,uwu-blue))))
                          `(org-habit-ready-face ((t :background ,uwu-green)))
                          `(org-habit-alert-face ((t :background ,uwu-yellow :foreground ,uwu-bg)))
                          `(org-habit-clear-face ((t :background ,uwu-blue)))
                          `(org-habit-overdue-face ((t :background ,uwu-red)))
                          `(org-habit-clear-future-face ((t :background ,uwu-blue)))
                          `(org-habit-ready-future-face ((t :background ,uwu-green)))
                          `(org-habit-alert-future-face ((t :background ,uwu-yellow :foreground ,uwu-bg)))
                          `(org-habit-overdue-future-face ((t :background ,uwu-red)))
                          ;;;;; org-ref
                          `(org-ref-ref-face ((t :underline t)))
                          `(org-ref-label-face ((t :underline t)))
                          `(org-ref-cite-face ((t :underline t)))
                          `(org-ref-glossary-face ((t :underline t)))
                          `(org-ref-acronym-face ((t :underline t)))))



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
