(add-to-list 'load-path "~/.config/emacs/scripts/")
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
