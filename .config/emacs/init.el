(org-babel-load-file
 (expand-file-name
  "config.el"
  user-emacs-directory))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds nil)
 '(flycheck-checker-error-threshold nil)
 '(org-agenda-files
   '("~/Notes/Tasks.org" "/home/nightwing/.dotfiles/.config/emacs/README.org"))
 '(org-babel-load-languages
   '((awk . t)
	 (emacs-lisp . t)
	 (awk . t)
	 (gnuplot . t)
	 (python . t)
	 (haskell . t)
	 (shell . t)
	 (R . t)
	 (C . t)
	 (calc . t)))
 '(package-selected-packages '(mugur darkroom arxiv-mode biblio)))
