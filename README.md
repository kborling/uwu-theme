# uwu-theme.el
An awesome dark color scheme for emacs.

## Screenshot
![UwU emacs theme](https://github.com/kborling/uwu.el/blob/main/uwu.png)

## Try It

1. Clone repo `git clone https://github.com/kborling/uwu.el.git` in directory of your choosing.
2. Type `M-x load-file` and choose the `uwu-theme.el` file.
3. Type `M-x enable-theme` and choose `uwu`.

## Installation

1. Clone repo `git clone https://github.com/kborling/uwu.el.git` in directory of your choosing. 
2. Add the following code in your emacs config file:
   ```elisp
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ;;; Make a themes directory and add uwu.el to it

   (load-theme 'uwu t t)
   (enable-theme 'uwu)
   ```

## Acknowledgments
- Inspiration and Color Scheme based on https://github.com/Mangeshrex/uwu.vim
