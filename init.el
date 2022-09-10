;; (setq package-list '(rust-playground rust-mode org-pdftools yasnippet-snippets multi-term org-pdfview telega vterm pdf-tools js3-mode prettier-js org-superstar quelpa tree-sitter-ispell yasnippet-lean react-snippets use-package yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard fic-mode rust-mode rust-playground)) ;

;; xah-fly-keys 

(package-initialize)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; ; fetch the list of packages available 
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

(load "~/.emacs.d/setting-packages")
(load "~/.emacs.d/setting-lsp")
(load "~/.emacs.d/setting-font-face")
;(load "~/.emacs.d/company-go")

(load-theme 'gruvbox-dark-medium t)

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

(setq make-backup-files nil)          ; Delete #filename# files

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

																				
(dolist (mode '(org-mode-hook ; Disable line numbers for some modes
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq gc-cons-threshold most-positive-fixnum) ; Minimize garbage collection during startup

(add-hook 'emacs-startup-hook ; Lower threshold back to 8 MiB (default is 800kB)
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
																				; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-auto-revert-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ctrl+C, Ctrl+V copy, paste mode
;(global-set-key (kbd "C-c") 'kill-ring-save)
;(global-set-key (kbd "C-v") 'yank)

;;;; Custom modeline ;;;;

;;(defun mode-line-fill (face reserve)
;;  "Return empty space using FACE and leaving RESERVE space on the right."
;;  (when
;;      (and window-system (eq 'right (get-scroll-bar-mode)))
;;    (setq reserve (- reserve 3)))
;;  (propertize " "
;;              'display
;;              `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
;;              'face face
;;              )
;;  )
;;
;;(use-package nyan-mode
;;  :config
;;  (setq nyan-bar-length 15)
;;  )
;;(require 'nyan-mode)
;;
;;;;;(let (sndr-font-base (cdr (assoc "zenbu-salmon" zenbu-colors-alist)) )
;;;; ■◧  ▥
;;
;;(defun get-evil-state-icons()
;;  (interactive)
;;  (cond
;;   ((memq evil-state '(emacs))
;;    (propertize "⭘ ⭘" 'face '((:foreground "orange" :weight bold )))
;;    )
;;
;;   ((memq evil-state '(hybrid insert))
;;    (propertize " " 'face '((:foreground "green" :weight bold )))
;;    )
;;
;;   ((memq evil-state '(visual))
;;    (propertize "⭘ ⭘" 'face '((:foreground "red" :weight bold )))
;;    )
;;
;;   (t
;;    (propertize "⭘ ⭘" 'face '((:weight ultra-light )))
;;    )
;;   )
;;  )
;;
;;(setq-default mode-line-format
;;              (list
;;               " "
;;               '(:eval (get-evil-state-icons) );; end evil-state
;;               " "
;;               '(:eval (when buffer-read-only
;;                         (propertize " " 'help-echo "Buffer is read-only")))
;;               '(:eval
;;                 (if (buffer-modified-p)
;;                     (propertize " %b " 'face '((:weight bold )) 'help-echo (buffer-file-name) )
;;                   (propertize "%b " 'help-echo (buffer-file-name))
;;                   ))
;;               (propertize " · " 'face 'font-lock-type-face)
;;               ;; '%02' to set to 2 chars at least; prevents flickering
;;               (propertize "%02l, %02c" 'face 'font-lock-type-face)
;;               ;; the current major mode for the buffer.
;;               (propertize " · %m ·" 'face 'font-lock-type-face)
;;               mode-line-misc-info
;;               (propertize " · " 'face 'font-lock-type-face)
;;               (mode-line-fill 'mode-line 30)
;;               (propertize " · " 'face 'font-lock-type-face)
;;               '(:eval (list (nyan-create)))
;;               (propertize " · " 'face 'font-lock-type-face)
;;               '(:eval (if-let (vc vc-mode)
;;                           (list "  " (substring vc 5))
;;                         (list "        " )
;;                         ))
;;               ))


(custom-set-variables
 '(custom-safe-themes
	 '(default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(rust-playground rust-mode org-pdftools yasnippet-snippets multi-term org-pdfview telega vterm pdf-tools js3-mode prettier-js org-superstar quelpa tree-sitter-ispell xah-fly-keys yasnippet-lean react-snippets use-package yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-mode helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard fic-mode rust-mode rust-playground))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
