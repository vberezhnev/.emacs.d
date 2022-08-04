;; (package-initialize)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-selected-packages '(all-the-icons ivy doom-themes auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

;; Set up package.el to work with MELPA
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (package-refresh-contents)



(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(add-to-list 'load-path "~/.emacs.d/evil")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab"))
(add-to-list 'load-path "~/.emacs.d/neotree")
(add-to-list 'load-path "~/.emacs.d/powerline")

(require 'evil)
(require 'use-package)
(require 'awesome-tab)
(require 'elcord)
(require 'lsp-mode)
(require 'powerline)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(evil-mode 1)
(elcord-mode)
(require 'neotree)
(require 'powerline)

(setq make-backup-files nil)

;; Company mode
;(setq company-idle-delay 0)
;(setq company-minimum-prefix-length 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setting TreeMacs
(global-set-key (kbd "C-x C-m") 'treemacs)
(global-set-key [f8] 'treemacs)

(global-auto-revert-mode t)

(use-package treemacs
	:config
	(progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0))
		      treemacs-show-hidden-files               -1
		)
	
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Setting packages ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package projectile
  :demand t
  :init (projectile-global-mode 1)
  :bind-keymap* ("C-x p" . projectile-command-map)
  :config
  (require 'projectile)
  (use-package counsel-projectile 
    :bind (("s-p" . counsel-projectile)
           ("s-f" . counsel-projectile-find-file)
           ("s-b" . counsel-projectile-switch-to-buffer)))
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy))    

; Deleting top bar
(menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

(setq-default tab-width 2)

;; Restore previous session
(desktop-save-mode 1)

;; Load theme
(load-theme 'gruvbox t)
(awesome-tab-mode t)

(powerline-default-theme)

;; Ctrl+C, Ctrl+V copy, paste mode
;(global-set-key (kbd "C-c") 'kill-ring-save)
;(global-set-key (kbd "C-v") 'yank)


;; Setting tabs
(when (not (display-graphic-p))
  (setq frame-background-mode 'dark))
(setq awesome-tab-label-fixed-length 16)
(setq awesome-tab-height 90)
(setq awesome-tab-show-tab-index t)

;; Setting dashboard
;; (setq dashboard-startup-banner "~/Изображения/Logos/dailyminimal/Olivia Black.jpeg")
(setq dashboard-banner-logo-title "Welcome back, Vladimir!")
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(dashboard-setup-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; LSP ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(inhibit-startup-screen t)

;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a" default))
 '(package-selected-packages
	 '(## doom-themes all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
