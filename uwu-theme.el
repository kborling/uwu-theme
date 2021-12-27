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

;;; Theme Faces
(custom-theme-set-faces 'uwu
                        '(font-lock-keyword-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#b185db"))))
                        '(font-lock-function-name-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#53A7BF"))))
                        '(font-lock-string-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#6bb05d"))))
                        '(font-lock-warning-face
                          ((((class color)
                             (min-colors 89))
                            (:inverse-video t :background "Red" :foreground "#f65b5b"))))
                        '(underline
                          ((((class color)
                             (min-colors 89))
                            (:underline t :foreground "#e59e67"))))
                        '(font-lock-type-face
                          ((((class color)
                             (min-colors 89))
                            (:weight bold :foreground "#e59e67"))))
                        '(font-lock-preprocessor-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#53A7BF"))))
                        '(font-lock-builtin-face
                          ((((class color)
                             (min-colors 89))
                            (:weight bold :foreground "#e59e67"))))
                        '(font-lock-variable-name-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#f65b5b"))))
                        '(font-lock-constant-face
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#e59e67"))))
                        '(font-lock-doc-face
                          ((((class color)
                             (min-colors 89))
                            (:slant italic :foreground "#62686A"))))
                        '(font-lock-comment-face
                          ((((class color)
                             (min-colors 89))
                            (:slant italic :foreground "#62686A"))))
                        '(show-paren-match-face
                          ((((class color)
                             (min-colors 89))
                            (:inverse-video t :background "Cyan" :foreground "#f65b5b"))))
                        '(default
                           ((((class color)
                              (min-colors 89))
                             (:background "#131A1C" :foreground "#C5C8C9"))))
                        '(cursor
                           ((((class color)
                              (min-colors 89))
                             (:background "#d6d6d6" :foreground "#232a3c"))))
                        '(highline-face
                          ((((class color)
                             (min-colors 89))
                            (:background "#1b2224"))))
                        '(ac-selection-face
                          ((((class color)
                             (min-colors 89))
                            (:background "#b185db" :foreground "#2f3638"))))
                        '(ac-candidate-face
                          ((((class color)
                             (min-colors 89))
                            (:background "#1b2224" :foreground "#c4c4c4"))))
                        '(flyspell-duplicate
                          ((((class color)
                             (min-colors 89))
                            (:underline
                             (:style wave)
                             :foreground "#b185db"))))
                        '(flyspell-incorrect
                          ((((class color)
                             (min-colors 89))
                            (:underline
                             (:style wave)
                             :foreground "#f65b5b"))))
                        '(link
                          ((((class color)
                             (min-colors 89))
                            (:underline t :foreground "#53A7BF"))))
                        '(minibuffer-prompt
                          ((((class color)
                             (min-colors 89))
                            (:foreground "#e59e67"))))
                        '(highlight
                          ((((class color)
                             (min-colors 89))
                            (:background "#2f3638"))))
                        '(region
                          ((((class color)
                             (min-colors 89))
                            (:inverse-video t :background "#1b2224"))))
                        '(mode-line-inactive
                          ((((class color)
                             (min-colors 89))
                            (:background "#1B2224" :foreground "#C5C8C9"))))
                        '(mode-line-buffer-id
                          ((((class color)
                             (min-colors 89))
                            (:weight bold :background "#1B2224" :foreground "#C5C8C9"))))
                        '(mode-line
                          ((((class color)
                             (min-colors 89))
                            (:weight bold :background "#1B2224" :foreground "#C5C8C9"))))
                        '(fringe
                          ((((class color)
                             (min-colors 89))
                            (:underline t :foreground "#2f3638"))))
                        '(linum
                          ((((class color)
                             (min-colors 89))
                            (:underline t :foreground "#2f3638"))))
                        '(isearch
                          ((((class color)
                             (min-colors 89))
                            (:inverse-video t :background "#2f3638" :foreground "#53A7BF"))))
                        '(isearch-lazy-highlight-face
                          ((((class color)
                             (min-colors 89))
                            (:inverse-video t :foreground "#e59e67"))))
                        '(dired-directory
                          ((((class color)
                             (min-colors 89))
                            (:weight bold :foreground "#53A7BF")))))

;;; Footer

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
