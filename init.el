;; (package-initialize)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (setq package-selected-packages '(all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))

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
;(add-to-list 'load-path "~/.emacs.d/neotree")
(add-to-list 'load-path "~/.emacs.d/powerline")

(require 'evil)
(require 'use-package)
(require 'awesome-tab)
(require 'elcord)
(require 'powerline)

(require 'lsp-mode)

(when (display-graphic-p)
  (require 'all-the-icons))

(use-package all-the-icons
  :if (display-graphic-p))

(evil-mode 1)
(elcord-mode)
(require 'neotree)
(require 'powerline)

(setq make-backup-files nil)          ; Delete #filename# files
;(desktop-save-mode 1)                ; A global mode that automatically saves your Emacs session
(display-line-numbers-mode)           ; Display numbers
(setq display-line-numbers 'relative) ; Set relative numbers
(xterm-mouse-mode 1)                  ; Mouse support for terminal

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Disabling the *scratch* buffer - thus disabling it on startup.
(setq initial-major-mode (quote fundamental-mode))

;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook 
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

;; Company mode
;(setq company-idle-delay 0)
;(setq company-minimum-prefix-length 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-revert-mode t)

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree)
          ; Every time when the neotree window is opened, let it find current file and jump to node.
          (setq neo-smart-open t)
          ; Do not autorefresh directory to show current file
          (setq neo-autorefresh nil))


(use-package treemacs
	:config
	(progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0))
		      treemacs-show-hidden-files               nil
		)
	
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Setting packages ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq-default tab-width 2) ; set default tab char's display width to 2 spaces
(setq tab-width 2)         ; set current buffer's tab char's display width to 2 spaces

;; Restore previous session
(desktop-save-mode 1)

;; Load theme
;(load-theme 'gruvbox t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; My Funcs ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defun my-insert-tab-char ()
;   "insert a tab char. (ASCII 9, \t)"
;   (interactive)
;   (insert "\t")
; 	)
; 
; (global-set-key (kbd "TAB") 'my-insert-tab-char)
; (global-set-key (kbd "<tab>") 'my-insert-tab-char)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; LSP ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(lsp-treemacs-sync-mode 1)

; Install company
(use-package company
  :ensure t
  :config (add-hook 'prog-mode-hook 'company-mode)
          (global-set-key (kbd "M-i") 'company-complete))
(use-package company-anaconda 
  :defer
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

; Company language package for PHP
(use-package company-php
  :defer
  :after company)

; Just as an example, aso Ruby:
(use-package robe ;; company-robe is a Ruby mode
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-robe)
          (add-hook 'ruby-mode-hook 'robe-mode))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; for completions
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))


(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

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
	 '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a" default))
 '(package-selected-packages
	 '(## spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
