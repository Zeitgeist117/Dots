(add-to-list 'load-path "~/.config/emacs/scripts/")
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/nightwing/Notes/org/"
	 "/home/nightwing/Notes/roam/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(breadcrumb-face ((t (:height 0.8))))
 '(breadcrumb-imenu-leef-face ((t (:height 1.0))))
 '(breadcrumb-project-leef-face ((t (:height 0.8))))
 '(fixed-pitch ((t (:inherit default))))
 '(line-number ((t (:height 0.8 :inherit shadow))))
 '(line-number-current-line ((t (:inherit line-number))))
 '(mode-line ((t (:height 0.9))))
 '(mode-line-inactive ((t (:inherit mode-line))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "#83a598" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(tab-bar ((t (:height 0.9)))))
