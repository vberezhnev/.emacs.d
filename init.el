;; (package-initialize)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (setq package-selected-packages '(all-the-icons auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))

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
(require 'awesome-tab)
(require 'elcord)
(require 'lsp-mode)
(require 'powerline)
(require 'use-package)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(evil-mode 1)
(elcord-mode)
(require 'neotree)
(require 'powerline)

;; Company mode
;(setq company-idle-delay 0)
;(setq company-minimum-prefix-length 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-m") 'treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Setting packages ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Deleting top bar
(menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

(setq-default tab-width 2)

;; Load theme
(load-theme 'gruvbox t)
(awesome-tab-mode t)

(powerline-default-theme)

;; Ctrl+C, Ctrl+V copy, paste mode
;(global-set-key (kbd "C-c") 'kill-ring-save)
;(global-set-key (kbd "C-v") 'yank)

;; Setting TreeMacs
(global-set-key [f8] 'treemacs-toggle)

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

;; LSP for Golang
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)

 (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
