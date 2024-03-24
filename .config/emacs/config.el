(add-to-list 'load-path "~/.config/emacs/scripts/")

;;(require 'elpaca-setup)
  (require 'buffer-move)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives


'("melpa-stable" . "https://stable.melpa.org/packages/") t)

(use-package evil
	:init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-vsplit-window-right t)
	(evil-mode))  
(use-package evil-collection
	:after evil
	:config
	(setq evil-collection-mode-list '(dashboard dired ibuffer pdf))
	(evil-collection-init))
(use-package evil-tutor)
;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
	(define-key evil-motion-state-map (kbd "SPC") nil)
	(define-key evil-motion-state-map (kbd "RET") nil)
	(define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
	(setq org-return-follows-link  t)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
			 (setq-local electric-pair-inhibit-predicate
					 `(lambda (c)
					(if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)  ;; Enable truncated lines
(menu-bar-mode -1)           ;; Disable the menu bar 
(scroll-bar-mode -1)         ;; Disable the scroll bar
(tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files"))) ;; change backup saves location to trash folder
(setq split-width-threshold nil)
;; Remove messages from the *Messages* buffer.
(setq-default message-log-max nil)
;; Kill both buffers on startup.
(kill-buffer "*Messages*")
;; Empty the *scratch* buffer.
(setq initial-scratch-message "")
(kill-buffer "*scratch*")
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq bookmark-save-flag 1) ; save bookmarks automatically

(use-package general
	:config
	(general-evil-setup)

	;; set up 'SPC' as the global leader key
	(general-create-definer zg/leader-keys
	  :states '(normal insert visual emacs)
	  :keymaps 'override
	  :prefix "SPC" ;; set leader
	  :global-prefix "M-SPC") ;; access leader in insert mode

	(zg/leader-keys
	  "SPC" '(counsel-M-x :wk "Counsel M-x")
	  "." '(find-file :wk "Find file")
	  "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
	  "c c" '(comment-line :wk "Comment lines")
	  "u" '(universal-argument :wk "Universal argument"))

	(zg/leader-keys
	  "f" '(:ignore t :wk "Files")    
	  "f c" '((lambda () (interactive)
				(find-file "~/.config/emacs/README.org")) 
			  :wk "Open emacs README.org")
	  "f e" '((lambda () (interactive)
				(dired "~/.config/emacs/")) 
			  :wk "Open user-emacs-directory in dired")
	  "f d" '(find-grep-dired :wk "Search for string in files in DIR")
	  "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
	  "f i" '((lambda () (interactive)
				(find-file "~/.config/emacs/init.el")) 
			  :wk "Open emacs init.el")
	  "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
	  "f l" '(counsel-locate :wk "Locate a file")
	  "f r" '(counsel-recentf :wk "Find recent files")
	  "f u" '(sudo-edit-find-file :wk "Sudo find file")
	  "f U" '(sudo-edit :wk "Sudo edit file"))

	(zg/leader-keys
	  "b" '(:ignore t :wk "Bookmarks/Buffers")
	  "b b" '(switch-to-buffer :wk "Switch to buffer")
	  "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
	  "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
	  "b d" '(bookmark-delete :wk "Delete bookmark")
	  "b i" '(ibuffer :wk "Ibuffer")
	  "b k" '(kill-current-buffer :wk "Kill current buffer")
	  "b K" '(kill-some-buffers :wk "Kill multiple buffers")
	  "b l" '(list-bookmarks :wk "List bookmarks")
	  "b m" '(bookmark-set :wk "Set bookmark")
	  "b n" '(next-buffer :wk "Next buffer")
	  "b p" '(previous-buffer :wk "Previous buffer")
	  "b r" '(revert-buffer :wk "Reload buffer")
	  "b R" '(rename-buffer :wk "Rename buffer")
	  "b s" '(basic-save-buffer :wk "Save buffer")
	  "b S" '(save-some-buffers :wk "Save multiple buffers")
	  "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

	(zg/leader-keys
	  "e" '(:ignore t :wk "Eshell/Evaluate")    
	  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
	  "e d" '(eval-defun :wk "Evaluate defun containing or after point")
	  "e e" '(eval-expression :wk "Evaluate and elisp expression")
	  "e h" '(counsel-esh-history :which-key "Eshell history")
	  "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
	  "e r" '(eval-region :wk "Evaluate elisp in region")
	  "e R" '(eww-reload :which-key "Reload current page in EWW")
	  "e s" '(eshell :which-key "Eshell")
	  "e w" '(eww :which-key "EWW emacs web wowser"))

	(zg/leader-keys
	  "g" '(:ignore t :wk "Git")    
	  "g /" '(magit-displatch :wk "Magit dispatch")
	  "g p" '(magit-push :wk "Magit push")
	  "g ." '(magit-file-displatch :wk "Magit file dispatch")
	  "g b" '(magit-branch-checkout :wk "Switch branch")
	  "g c" '(:ignore t :wk "Create") 
	  "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
	  "g c c" '(magit-commit-create :wk "Create commit")
	  "g c f" '(magit-commit-fixup :wk "Create fixup commit")
	  "g C" '(magit-clone :wk "Clone repo")
	  "g f" '(:ignore t :wk "Find") 
	  "g f c" '(magit-show-commit :wk "Show commit")
	  "g f f" '(magit-find-file :wk "Magit find file")
	  "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
	  "g F" '(magit-fetch :wk "Git fetch")
	  "g g" '(magit-status :wk "Magit status")
	  "g i" '(magit-init :wk "Initialize git repo")
	  "g l" '(magit-log-buffer-file :wk "Magit buffer log")
	  "g r" '(vc-revert :wk "Git revert file")
	  "g s" '(magit-stage-file :wk "Git stage file")
	  "g t" '(git-timemachine :wk "Git time machine")
	  "g u" '(magit-stage-file :wk "Git unstage file"))

	 (zg/leader-keys
	  "h" '(:ignore t :wk "Help")
	  "h f" '(describe-function :wk "Describe function")
	  "h v" '(describe-variable :wk "Describe variable")
	  "h t" '(load-theme :wk "Load theme")    
	  "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))

	 (zg/leader-keys
	   "m" '(:ignore t :wk "Org")
	   "m a" '(org-agenda :wk "Org agenda")
	   "m e" '(org-export-dispatch :wk "Org export dispatch")
	   "m i" '(org-toggle-item :wk "Org toggle item")
	   "m t" '(org-todo :wk "Org todo")
	   "m B" '(org-babel-tangle :wk "Org babel tangle")
	   "m T" '(org-todo-list :wk "Org todo list")
	   "m x" '(org-toggle-checkbox :wk "Org toggle checkbox")
	   "m l" '(org-roam-buffer-toggle :wk "Org Roam find node")
	   "m f" '(org-roam-node-find :wk "Org Roam find node")
	   "m I" '(org-roam-node-insert :wk "Org Roam insert node"))

	 (zg/leader-keys
	   "m b" '(:ignore t :wk "Tables")
	   "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

	 (zg/leader-keys
	   "m" '(:ignore t :wk "Org")
	   "m a" '(org-agenda :wk "Org agenda")
	   "m e" '(org-export-dispatch :wk "Org export dispatch")
	   "m t" '(org-todo :wk "Org todo")
	   "m B" '(org-babel-tangle :wk "Org babel tangle")
	   "m T" '(org-todo-list :wk "Org todo list")
	   "m d t" '(org-time-stamp :wk "Org time stamp"))

 
	 (zg/leader-keys
	  "c" '(:ignore t :wk "Schedule") 
	  "c s" '(org-schedule :wk "Set Org Schedule")
	  "c d" '(org-deadline :wk "Set Org Deadline")
	   )

	 (zg/leader-keys
	   "p" '(projectile-command-map :wk "Projectile"))

	 (zg/leader-keys
	  "t" '(:ignore t :wk "Toggle")
	  "t e" '(eshell-toggle :wk "Toggle eshell")
	  "t f" '(flycheck-mode :wk "Toggle flycheck")
	  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
	  "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
	  "t o" '(org-mode :wk "Toggle org mode")
	  "t r" '(rainbow-mode :wk "Toggle rainbow mode")
	  "t t" '(visual-line-mode :wk "Toggle truncated lines")
	  "t v" '(vterm-toggle :wk "Toggle vterm")
	  "t d" '(darkroom-mode :wk "Toggle darkroom"))

	 (zg/leader-keys
	  "s" '(:ignore t :wk "Search")
	  "s d" '(dictionary-search :wk "Search dictionary")
	  "s m" '(man :wk "Man pages")
	  "s t" '(tldr :wk "Lookup TLDR docs for a command")
	  "s w" '(woman :wk "Similar to man but doesn't require man"))

	(zg/leader-keys
	   "d" '(:ignore t :wk "Dired")
	   "d d" '(dired :wk "Open dired")
	   "d j" '(dired-jump :wk "Dired jump to current")
	   "d n" '(neotree-dir :wk "Open directory in neotree")
	   "d p" '(peep-dired :wk "Peep-dired"))

	(zg/leader-keys
	  "o" '(:ignore t :wk "Open")
	  "o d" '(dashboard-open :wk "Dashboard")
	  "o e" '(elfeed :wk "Elfeed RSS")
	  "o f" '(make-frame :wk "Open buffer in new frame")
	  "o F" '(select-frame-by-name :wk "Select frame by name"))

	 (zg/leader-keys
	  "w" '(:ignore t :wk "Windows")
	  ;; Window splits
	  "w c" '(evil-window-delete :wk "Close window")
	  "w n" '(evil-window-new :wk "New window")
	  "w s" '(evil-window-split :wk "Horizontal split window")
	  "w v" '(evil-window-vsplit :wk "Vertical split window")
	  ;; Window motions
	  "w h" '(evil-window-left :wk "Window left")
	  "w j" '(evil-window-down :wk "Window down")
	  "w k" '(evil-window-up :wk "Window up")
	  "w l" '(evil-window-right :wk "Window right")
	  "w w" '(evil-window-next :wk "Goto next window")
	  ;; Move Windows
	  "w H" '(buf-move-left :wk "Buffer move left")
	  "w J" '(buf-move-down :wk "Buffer move down")
	  "w K" '(buf-move-up :wk "Buffer move up")
	  "w L" '(buf-move-right :wk "Buffer move right"))
	 (zg/leader-keys
	  "a" '(:ignore t :wk "PDFs") 
	  "a i" '(pdf-annot-add-text-annotation :wk "insert text annotation in pdf") 
	   )
)

(use-package git-timemachine
      :after git-timemachine
      :hook (evil-normalize-keymaps . git-timemachine-hook)
      :config
	(evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
	(evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)
(use-package magit)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package all-the-icons
      :ensure t
      :if (display-graphic-p))

(use-package all-the-icons-dired
      :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package rainbow-mode
      :diminish
      :hook 
      ((org-mode prog-mode) . rainbow-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-visual-line-mode t)

(use-package which-key
      :init
	(which-key-mode 1)
      :config
      (setq which-key-side-window-location 'bottom
	      which-key-sort-order #'which-key-key-order-alpha
	      which-key-sort-uppercase-first nil
-add-column-padding 1
-max-display-columns nil
	      which-key-min-display-lines 6
	      which-key-side-window-slot -10
	      which-key-side-window-max-height 0.25
	      which-key-idle-delay 0.8
	      which-key-max-description-length 25
	      which-key-allow-imprecise-window-fit nil
	      which-key-separator " → " ))

(use-package counsel
      :after ivy
      :diminish
      :config (counsel-mode))

(use-package ivy
      :bind
      ;; ivy-resume resumes the last Ivy-based completion.
      (("C-c C-r" . ivy-resume)
       ("C-x B" . ivy-switch-buffer-other-window))
      :custom
      (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "(%d/%d) ")
      (setq enable-recursive-minibuffers t)
      :diminish
      :config
      (ivy-mode))

(use-package all-the-icons-ivy-rich
      :ensure t
      :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
      :after ivy
      :ensure t
      :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
      :custom
      (ivy-virtual-abbreviate 'full
       ivy-rich-switch-buffer-align-virtual-buffer t
       ivy-rich-path-style 'abbrev)
      :config
      (ivy-set-display-transformer 'ivy-switch-buffer
							       'ivy-rich-switch-buffer-transformer))

(use-package drag-stuff)

(use-package pdf-tools
  :defer t
  :commands pdf-loader-install
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
			  ("J" . pdf-view-next-line-or-next-page)
			  ("K" . pdf-view-previous-line-or-previous-page)
			  ("C-=" . pdf-view-enlarge)
			  ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf")
)
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(evil-set-initial-state 'pdf-view-mode 'normal)

(use-package toc-org
	:commands toc-org-enable
	:init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-superstar)
(setq
    org-superstar-headline-bullets-list '("⁖" "⁖" "⁖" "⁖" "⁖")
)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-ellipsis " ≫");;

(use-package olivetti
  :ensure t
  :config
  (message "Olivetti configuration loaded")
  (setq-default olivetti-body-width 110))

(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(defun org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom ;; disable a bunch of shit i find useless
 (org-modern-todo nil)
 (org-modern-todo-faces nil)
 (org-modern-date nil)
 (org-modern-date-active nil)
 (org-modern-date-inactive nil)
 (org-modern-done nil)
 (org-modern-label nil)
 (org-modern-agenda nil)
 (org-modern-timestamp nil)
 (org-modern-progress nil)
 (org-modern-progress-faces nil)
 (org-modern-priority nil)
 (org-modern-priority-faces nil)
 (org-modern-symbol nil)
 (org-modern-statistics nil)
 (org-modern-tags nil)
 (org-modern-faces nil)
 (org-modern-label-border nil)
)

(require 'svg-setup)

(use-package org-noter)

(require 'org-tempo)

(use-package biblio)
(use-package org-ref)
(setq biblio-download-directory "~/Downloads/")

(use-package org-roam
	:ensure t
	:custom
	(org-roam-directory "~/Notes/roam")
	:config
	(org-roam-setup)
)
(setq org-roam-db-autosync-mode t)

(use-package org-present)

(setq org-agenda-files 
	'("~/Notes/Tasks.org"))

(use-package org-auto-tangle
:defer t
:hook (org-mode . org-auto-tangle-mode)
:config
(setq org-auto-tangle-default t)
)

(use-package projectile
      :diminish
      :config
      (projectile-mode 1))

(use-package haskell-mode)
(use-package lua-mode)
(use-package yuck-mode)
(use-package markdown-mode)

(use-package diminish)

(use-package flycheck
:ensure t
:defer t
:diminish
:init (global-flycheck-mode))

(use-package company
      :defer 2
      :diminish
      :custom
      (company-begin-commands '(self-insert-command))
      (company-idle-delay .1)
      (company-minimum-prefix-length 2)
      (company-show-numbers t)
      (company-tooltip-align-annotations 't)
      (global-company-mode t))

(use-package company-box
      :after company
      :diminish
      :hook (company-mode . company-box-mode))



(use-package vterm
:config
(setq shell-file-name "/bin/sh"
	      vterm-max-scrollback 5000))

(use-package vterm-toggle
      :after vterm
      :config
      (setq vterm-toggle-fullscreen-p nil)
      (setq vterm-toggle-scope 'project)
      (add-to-list 'display-buffer-alist
			       '((lambda (buffer-or-name _)
					 (let ((buffer (get-buffer buffer-or-name)))
					       (with-current-buffer buffer
						 (or (equal major-mode 'vterm-mode)
							 (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
				      (display-buffer-reuse-window display-buffer-at-bottom)
				      ;;(display-buffer-reuse-window display-buffer-in-direction)
				      ;;display-buffer-in-direction/direction/dedicated is added in emacs27
				      ;;(direction . bottom)
				      ;;(dedicated . t) ;dedicated is supported in emacs27
				      (reusable-frames . visible)
				      (window-height . 0.3))))

(use-package sudo-edit
      :config
	(zg/leader-keys
	      "f u" '(sudo-edit-find-file :wk "Sudo find file")
	      "f U" '(sudo-edit :wk "Sudo edit file")))

(use-package discover)
(use-package dired-open
	:config
	(setq dired-open-extensions '(;;("gif" . "sxiv")
								  ;;("jpg" . "sxiv")
								  ;;("png" . "sxiv")
								  ;;("pdf" . "zathura")
								  ("mkv" . "mpv")
								  ("mp4" . "mpv"))))

(use-package peep-dired
	:after dired
	:hook (evil-normalize-keymaps . peep-dired-hook)
	:config
	  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
	  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
	  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
	  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package neotree
      :config
      (setq neo-smart-open t
		neo-show-hidden-files t
		neo-window-width 25 
		neo-window-fixed-size nil
		inhibit-compacting-font-caches t
		projectile-switch-project-action 'neotree-projectile-action) 
		;; truncate long file names in neotree
		(add-hook 'neo-after-create-hook
		       #'(lambda (_)
			       (with-current-buffer (get-buffer neo-buffer-name)
				 (setq truncate-lines t)
				 (setq word-wrap nil)
				 (make-local-variable 'auto-hscroll-mode)
				 (setq auto-hscroll-mode nil)))))
;; show hidden files

(global-set-key (kbd "M-k") 'next-buffer)
(global-set-key (kbd "M-j") 'previous-buffer)

(use-package centaur-tabs
	:demand
	:config
   (centaur-tabs-mode t)
   (setq centaur-tabs-group-buffer-group -1)
;;  :bind
;;(:map evil-normal-state-map
;;  ("M-k" . centaur-tabs-forward)
;;  ("M-j" . centaur-tabs-backward))
)
  (setq centaur-tabs-set-icons t)
  (setopt centaur-tabs-buffer-groups-function (lambda () '("All")))

(use-package elfeed
      :config
      (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
		elfeed-feeds (quote
					       (("https://export.arxiv.org/api/query?search_query=extra+terrestrials+cat:astro-ph.CV" ET)
						("https://kbd.news/rss2.php" KBD News)
						("https://odysee.com/$/rss/@AlphaNerd:8" Mental Outlaw)))))


(use-package elfeed-goodies
      :init
      (elfeed-goodies/setup)
      :config
      )

(use-package dracula-theme)
(use-package gruvbox-theme)
(use-package darktooth-theme)
(use-package modus-themes)
(use-package ef-themes)
(use-package poet-theme)

(use-package doom-themes
      :config
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		doom-themes-enable-italic t) ; if nil, italics is universally disabled
      ;; Sets the default theme to load!!! 
      (load-theme 'doom-gruvbox t)
      ;; Enable custom neotree theme (all-the-icons must be installed!)
      (doom-themes-neotree-config)
      ;; Corrects (and improves) org-mode's native fontification.
      (doom-themes-org-config))

(use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:config
	(setq doom-modeline-height 35      ;; sets modeline height
		  doom-modeline-bar-width 5    ;; sets right bar width
		  doom-modeline-persp-name t   ;; adds perspective name to modeline
		  doom-modeline-persp-icon t
		  doom-modeline-enable-word-count t)) ;; adds folder icon next to persp name

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package spacious-padding)
(require 'spacious-padding)

;; These is the default value, but I keep it here for visiibility.
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

;; Set a key binding if you need to toggle spacious padding.
(define-key global-map (kbd "<f8>") #'spacious-padding-mode)

(use-package dashboard
	:ensure t 
	:init
	(setq initial-buffer-choice 'dashboard-open)
	(setq dashboard-set-file-icons t)
	(setq dashboard-startup-banner "/home/nightwing/.dots/.config/emacs/images/emacs-dash.txt")  ;; use custom image as banner
	;; (setq dashboard-startup-banner 'text)
	(setq dashboard-center-content t) ;; set to 't' for centered content
	(setq dashboard-items '((recents . 5)
							(agenda . 3)
							(projects . 3)))
	:config
	(dashboard-setup-startup-hook))

(use-package perspective)
