(require 'package)
;;; Set up package.el to work with MELPA and ELPA
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ;; ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(load "~/.emacs.d/setting-packages")
(load "~/.emacs.d/setting-lsp")
(load "~/.emacs.d/setting-font-face")
;(load "~/.emacs.d/company-go")

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

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
(setq display-line-numbers-type 'relative)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Keymap ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "e8882a809fb14f2307410a659846a06bfa58a2279ffb1f5aca0c3aecbcb6aaee" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(rust-playground rust-mode org-pdftools general yasnippet-snippets multi-term org-pdfview telega vterm pdf-tools js3-mode prettier-js org-superstar quelpa tree-sitter-ispell xah-fly-keys simple-modeline yasnippet-lean react-snippets yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-mode helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard fic-mode rust-mode rust-playground evil-collection evil-tutor))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
