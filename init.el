(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-refresh-contents)

;; Install use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

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
	 '(indent-guide no-littering xclip web-mode use-package typescript-mode treemacs-evil treemacs-all-the-icons tree-sitter-langs reverse-im projectile pdf-tools parrot org-superstar multi-term magit-todos lsp-ui lsp-ivy json-mode helm-lsp gruvbox-theme go-mode fzf format-all flycheck-inline evil-multiedit evil-collection elfeed doom-themes doom-modeline diredfl dashboard dap-mode company-statistics company-quickhelp company-box company-anaconda blamer apheleia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)))
 '(mode-line ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-active ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-inactive ((t (:family "Iosevka Aile" :height 0.95)))))
