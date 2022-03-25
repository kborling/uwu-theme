# UwU Theme
An awesome dark color scheme for emacs.

## Screenshot
![UwU emacs theme](https://github.com/kborling/uwu.el/blob/main/assets/uwu.png)

## Try It

1. Clone repo `git clone https://github.com/kborling/uwu-theme.git` in directory of your choosing.
2. Type `M-x load-file` and choose the `uwu-theme.el` file.
3. Type `M-x enable-theme` and choose `uwu`.

## Installation

### Using package-install
`uwu-theme` is available from [MELPA](https://melpa.org/#/uwu-theme), such that it can be installed directly via `package-install`.

1. `M-x: package-install RET uwu-theme RET`.
2. Add the following code in your emacs config file:
   ```elisp
   (load-theme 'uwu t t)
   (enable-theme 'uwu)
   ```

### Manual
1. Clone repo `git clone https://github.com/kborling/uwu-theme.git` in directory of your choosing.
2. Add the following code in your emacs config file:
   ```elisp
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ;;; Make a themes directory and add uwu-theme to it

   (load-theme 'uwu t t)
   (enable-theme 'uwu)
   ```

### Using straight.el
1. [Install straight.el](https://github.com/raxod502/straight.el#getting-started)
2. Add the following code in your emacs config file:
   ```elisp
   (straight-use-package
      '(uwu-theme :host github :repo "kborling/uwu-theme"))
   (require 'uwu-theme)
   (load-theme 'uwu t)
   ```

### Using use-package with straight.el
1. [Install straight.el](https://github.com/raxod502/straight.el#getting-started)
2. Install use-package
   ```elisp
   (straight-use-package 'use-package)
   ```
4. Add the following code in your emacs config file:
   ```elisp
   (use-package uwu-theme
      :straight (uwu-theme :host github :repo "kborling/uwu-theme")
      :config (load-theme 'uwu t))
   ```

### Using Doom Emacs packages.el
1. Add the following code in your `~/.doom.d/packages.el` file:
   ```elisp
   (package! uwu-theme
      :recipe (:host github :repo "kborling/uwu-theme"))
   ```
2. Add the following code in your `~/.doom.d/config.el` file:
   ```elisp
   (require 'uwu-theme)
   (load-theme 'uwu t)
   ```

### Using Spacemacs packages
1. Append the following inside your `~/.config/spacemacs` `dotspacemacs-additional-packages` variable.
    ```elisp
    (uwu-theme :location (recipe :fetcher github :repo "kborling/uwu-theme"))
    ```
2. prepend `'uwu'` to the theme variable:
   ```elisp
      dotspacemacs-themes '(uwu)
   ```

## Options

### Variable Pitch Font
If you prefer to use a variable pitch font for headlines, add the following option to your emacs config file:
```elisp
;; Set the variable pitch font
(set-face-attribute 'variable-pitch nil
                    :family "Roboto" :height 130 :weight 'semibold)
;; Enable the use of the variable pitch font
(setq uwu-use-variable-pitch 1)
```

### Scaled Org/Outline Headlines
By default, the Org/Outline headlines will look like the following:
![UwU scaled Org headlines](https://github.com/kborling/uwu.el/blob/main/assets/org_headlines.png)

If you'd like to scale the headlines (Level 1-8), you can enable scaled headlines to achieve the following:
![UwU scaled Org headlines](https://github.com/kborling/uwu.el/blob/main/assets/scaled_org_headlines.png)

To use scaled headlines, add the following option to your emacs config file:
```elisp
;; Scale org-mode headlines
(setq uwu-scale-org-headlines 1)
;; Scale outline-mode headlines
(setq uwu-scale-outline-headlines 1)
```

### Distinct Line Numbers
By default, the line numbers are distinct and look like the following:
![UwU distinct line numbers](https://github.com/kborling/uwu.el/blob/main/assets/distinct_line_numbers.png)

If you prefer a less distracting line number style, you can disable the distinct line numbers style to achieve the following:
![UwU subtle line numbers](https://github.com/kborling/uwu.el/blob/main/assets/subtle_line_numbers.png)

To use the less distracting line number style, add the following option to your emacs config file:
```elisp
(setq uwu-distinct-line-numbers 'nil)
```

## Acknowledgments
- Inspiration and Color Scheme based on https://github.com/Mangeshrex/uwu.vim
