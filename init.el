(require 'package)
;;; Set up package.el to work with MELPA and ELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-refresh-contents)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/neotree")
(add-to-list 'load-path "~/.emacs.d/company-go.el")

(load "~/.emacs.d/setting-packages")
(load "~/.emacs.d/setting-lsp")
(load "~/.emacs.d/setting-font-face")

(require 'company-go)
(require 'elcord)
(require 'neotree)

(require 'lsp-mode)
(require 'react-snippets)

(use-package all-the-icons
  :if (display-graphic-p))

(elcord-mode)

(setq telega-server-command (file-symlink-p (executable-find telega-server-command)))
(setq telega-use-docker t)

(setq make-backup-files nil)          ; Delete #filename# files
;(desktop-save-mode 1)                ; A global mode that automatically saves your Emacs session

(display-line-numbers-mode)           ; Display numbers
(xterm-mouse-mode 1)                  ; Mouse support for terminal

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

(setq-default tab-width 2) ; set default tab char's display width to 2 spaces
(setq tab-width 2)         ; set current buffer's tab char's display width to 2 spaces

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
;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(defun open-all-recent-files ()
     "Open all recent files."
     (interactive)
     (dolist (file  recentf-list) (find-file file)))

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
	 '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "d543a5f82ce200d50bdce81b2ecc4db51422439ba7c0e6845483dd89566e4cf9" "c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "5dbdb4a71a0e834318ae868143bb4329be492dd04bdf8b398fb103ba1b8c681a" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(quelpa tree-sitter-ispell telega xah-fly-keys simple-modeline yasnippet-lean react-snippets use-package yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode zenburn-theme ## spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
