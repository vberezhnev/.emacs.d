(require 'package)
;;; Set up package.el to work with MELPA and ELPA
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ;; ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)


(load "~/.emacs.d/setting-packages")
(load "~/.emacs.d/setting-lsp")
(load "~/.emacs.d/setting-font-face")

;(load "~/.emacs.d/company-go")

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

;(desktop-save-mode 1)                ; A global mode that automatically saves your Emacs session

(load-theme 'gruvbox-dark-medium t)

(setq make-backup-files nil)          ; Delete #filename# files
(setq telega-use-docker t)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(add-hook 'minibuffer-exit-hook
	'(lambda ()
           (let ((buffer "*Completions*"))
             (and (get-buffer buffer)
	  	(kill-buffer buffer)))))

(setq initial-major-mode (quote fundamental-mode))


(xterm-mouse-mode 1)

(setq-default tab-width 2) ; set default tab char's display width to 2 spaces
(setq tab-width 2)         ; set current buffer's tab char's display width to 2 spaces

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
      (lambda ()
        (setq gc-cons-threshold (expt 2 23))))


; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;(column-number-mode)
(global-display-line-numbers-mode t)
;(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

; Minimize garbage collection during startup
;(setq gc-cons-threshold most-positive-fixnum)

; Lower threshold back to 8 MiB (default is 800kB)
;(add-hook 'emacs-startup-hook
;          (lambda ()
;            (setq gc-cons-threshold (expt 2 23))))

;(defun open-all-recent-files ()
;  "Open all recent files."
;  (interactive)
;  (dolist (file  recentf-list) (find-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-revert-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "e8882a809fb14f2307410a659846a06bfa58a2279ffb1f5aca0c3aecbcb6aaee" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(org-pdftools yasnippet-snippets multi-term org-pdfview telega vterm pdf-tools js3-mode prettier-js org-superstar quelpa tree-sitter-ispell xah-fly-keys simple-modeline yasnippet-lean react-snippets use-package yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode zenburn-theme spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-mode helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard fic-mode))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
