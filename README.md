# UwU Theme
An awesome dark color scheme for emacs.

## Screenshot
![UwU emacs theme](https://github.com/kborling/uwu.el/blob/main/uwu.png)

## Try It

1. Clone repo `git clone https://github.com/kborling/uwu-theme.git` in directory of your choosing.
2. Type `M-x load-file` and choose the `uwu-theme.el` file.
3. Type `M-x enable-theme` and choose `uwu`.

## Installation

### Using package-install
`uwu-theme` is available from [[https://melpa.org/#/uwu-theme][MELPA]], such that it can be installed directly via `package-install`.

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

## Acknowledgments
- Inspiration and Color Scheme based on https://github.com/Mangeshrex/uwu.vim
