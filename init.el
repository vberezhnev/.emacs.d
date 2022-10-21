(require 'package)
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Clean up all those temporary files
(use-package no-littering)

(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(dired no-littering xclip web-mode use-package typescript-mode treemacs-evil treemacs-all-the-icons tree-sitter-langs reverse-im projectile pdf-tools parrot org-superstar multi-term magit-todos lsp-ui lsp-ivy json-mode helm-lsp gruvbox-theme go-mode fzf format-all flycheck-inline evil-multiedit evil-collection elfeed doom-themes doom-modeline diredfl dashboard dap-mode company-statistics company-quickhelp company-box company-anaconda blamer apheleia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)))
 '(mode-line ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-active ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-inactive ((t (:family "Iosevka Aile" :height 0.95)))))
