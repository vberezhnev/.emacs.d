(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-refresh-contents)

;; ;; Install straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

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
 '(custom-safe-themes
	 '("b4e786d88aeb48bce6c3b93a72d50e9c98966b759b2b09d837ea93e25acb8cc2" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f028e1985041fd072fa9063221ee9c9368a570d26bd6660edbd00052d112e8bb" "e8882a809fb14f2307410a659846a06bfa58a2279ffb1f5aca0c3aecbcb6aaee" default))
 '(package-selected-packages
	 '(atom-one-dark zoom yasnippet-snippets xwwp-follow-link-helm xclip which-key web-mode vue-mode vterm use-package typescript-mode treemacs-evil treemacs-all-the-icons tree-sitter-langs telega solo-jazz-theme rust-playground rust-mode reverse-im react-snippets quelpa projectile prettier-js powerline-evil parrot org-superstar org-preview-html org-pdftools org-make-toc nyan-mode multi-term mood-line moe-theme melancholy-theme lua-mode lsp-ui json-mode js3-mode js2-mode ivy helm-lsp gruvbox-theme go-mode fzf format-all flycheck fic-mode evil-tutor evil-multiedit evil-collection elcord ef-themes doom-themes doom-modeline dashboard dap-mode company centaur-tabs catppuccin-theme atom-one-dark-theme apheleia ample-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
