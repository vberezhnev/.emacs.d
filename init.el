(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

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

;; (load "~/.emacs.d/setting-packages")
;; (load "~/.emacs.d/setting-lsp")
;; (load "~/.emacs.d/setting-font-face")
;; (load "~/.emacs.d/setting-performance.el")

(load-theme 'atom-one-dark t)

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
	 '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "43b78a08f245bc198dadf35b324f445472c92dda3f1b6d1746cefee9f2ade177" "7f954d5bee47a27cc6cc83d1d6b80f7a32d82f744a725375d6e22b65758f9a5e" "8d68cd652ab405df5db91acc66be58307e008bfac3ddc4beef7d7f814d37026c" "df069ec238487ceab1cec64809a3c1dcef1393123ecdf430bdb7b94537ca2c6a" "92cfd42cedb42fdd3ea0d84d518825a94f29b30be20f65978dab7d7c8fa30c6a" "06a2eef27703cd3c8b017c90d9025d766ade307971826362c487a5273e14cc5a" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "b189e1cc9e17572cc8f0f7c21a1c7998c234936b40fba566ecaee49f2166ad0c" "b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a" "0ed3d96a506b89c1029a1ed904b11b5adcebeb2e0c16098c99c0ad95cb124729" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "58c996beb973f7e988ee4fd21c367b7a5bbdb0622ddfbbd112672a7b4e3d3b81" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "2627707bc15dd427ef165fc8ff9868e3e184f6a151f139c092561bbc39734364" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(org-make-toc org-preview-html evil-multiedit mood-line powerline-evil atom-one-dark-theme ef-themes nano-modeline nano-theme go-mode ample-theme melancholy-theme moe-theme lua-mode zoom nyan-mode parrot catppuccin-theme rust-playground rust-mode org-pdftools yasnippet-snippets multi-term org-pdfview telega vterm pdf-tools js3-mode prettier-js org-superstar quelpa tree-sitter-ispell xah-fly-keys yasnippet-lean react-snippets use-package yasnippet-classic-snippets doom-themes doom-modeline material-theme emmet-mode web-mode vue-mode spacemacs-theme typescript-mode all-the-icons ivy auto-complete monokai-theme elcord lsp-mode lsp-ui yasnippet lsp-treemacs helm-mode helm-lsp projectile hydra flycheck avy which-key helm-xref dap-mode gruvbox-theme json-mode dashboard fic-mode rust-mode rust-playground))
 '(warning-suppress-types
	 '((use-package)
		 (use-package)
		 (use-package)
		 (use-package)
		 (use-package)
		 (use-package))))
