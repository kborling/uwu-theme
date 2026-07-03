# UwU Theme

A dark color scheme for Emacs with broad package coverage and WCAG AA accessible contrast.

## Screenshot
![UwU emacs theme](https://github.com/kborling/uwu.el/blob/main/assets/uwu.png)

## Install

```elisp
;; MELPA
(use-package uwu-theme
  :config (load-theme 'uwu t))

;; Or from source
(use-package uwu-theme
  :vc (:url "https://github.com/kborling/uwu-theme" :rev :newest)
  :config (load-theme 'uwu t))
```

## Options

```elisp
(setq uwu-cursor-color 'red)              ; 'white (default) or 'red
(setq uwu-distinct-line-numbers t)         ; contrasting line number background
(setq uwu-use-variable-pitch t)           ; variable pitch for headings
(setq uwu-scale-org-headlines t)          ; scaled org headings
(setq uwu-scale-outline-headlines nil)
(setq uwu-scale-shr-headlines nil)
```

## Supported Packages

Core: font-lock (including treesit faces), mode-line, fringe, isearch, diff-mode, dired, eshell, flymake, flycheck, flyspell, show-paren, ediff, smerge

Completion: corfu, marginalia, orderless, completion-preview, icomplete, company, ivy, helm, vertico

Org: org-mode, org-modern, org-ref

Development: eglot, transient, which-key, diff-hl, vc-dir, log-view, magit

Terminal: term, eat, ansi-color

UI: tab-bar, tab-line, outline, rainbow-delimiters, hl-todo

Apps: notmuch, mu4e, gnus, message, shr, deft, gptel, deadgrep, helpful, multiple-cursors

Misc: sly, slime, eldoc, bookmark, powerline, spaceline, embark, consult, nano

## Accessibility

All foreground colors meet WCAG AA contrast ratio (4.5:1) against the background. Comment color was adjusted from `#62686A` to `#828A8C` to meet this standard.
