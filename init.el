;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)


(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab"))
(add-to-list 'load-path "~/.emacs.d/neotree")

(require 'evil)
(require 'use-package)
(require 'awesome-tab)
(require 'elcord)
(require 'neotree)

(require 'package)
(require 'lsp-mode)

(use-package all-the-icons
  :if (display-graphic-p))

(evil-mode 1)
(elcord-mode)

(setq make-backup-files nil)          ; Delete #filename# files
;(desktop-save-mode 1)                ; A global mode that automatically saves your Emacs session
(display-line-numbers-mode)           ; Display numbers
(setq display-line-numbers 'relative) ; Set relative numbers
(xterm-mouse-mode 1)                  ; Mouse support for terminal

;; Make *scratch* buffer blank.
(setq initial-scratch-message nil)

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

;; JSX syntax highlighting
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-revert-mode t)

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle)
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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
	:commands doom-modeline
	:config
  ;(setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  ;(setq doom-modeline-minor-modes (featurep 'minions))
  ;(setq doom-modeline-minor-modes (featurep 'minions))
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-set-timemachine-modeline)
  :hook (after-init . doom-modeline-mode)
)

(use-package all-the-icons
      :config
      ;; Make sure the icon fonts are good to go
      (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
      (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
      (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
      (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
      (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
      (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

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
;(desktop-save-mode 1)

;; Load theme
(awesome-tab-mode t)


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

(add-hook 'web-mode-hook  'emmet-mode)

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

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
         (go-mode . lsp)
         (javascript-mode . lsp)
         (typescript-mode . lsp)
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
;(use-package company-lsp
;  :after lsp-mode
;  :config (push 'company-lsp company-backends))

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))


(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "d543a5f82ce200d50bdce81b2ecc4db51422439ba7c0e6845483dd89566e4cf9" "c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode zenburn-theme ## spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))
 '(warning-suppress-types '((use-package))))

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(font-lock-comment-face ((t nil)))
 ;; '(widget-field ((t (:extend t :background "midnightblue" :foreground "azure" :width normal)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
