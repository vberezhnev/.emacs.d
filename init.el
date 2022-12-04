(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
				("ORG"		. "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("ORG"		. 20)
				("MELPA"        . 15)
				("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)))

(package-initialize)
;;(package-refresh-contents)

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
	 '("9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "b290c815faa375c1e84973e8c71309459d84e33ad51ded96667f6b62027d8ce8" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" default))
 '(helm-minibuffer-history-key "M-p")
 '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))
 '(org-agenda-files
	 '("/home/beethoven/Org/school.org" "/home/beethoven/Org/home.org" "/home/beethoven/Org/work.org"))
 '(package-selected-packages
	 '(theme-changer rust-auto-use proof-general consult-company company-web general company-nixos-options clang-format org-caldav rjsx-mode org-wild-notifier org-alert org-modern pdf-view-restore git-gutter import-js modus-themes backlight dmenu ivy-posframe djvu emojify vterm cpupower simple-modeline nixpkgs-fmt nix-mode exwm-modeline emms playerctl lua-mode all-the-icons-dired good-scroll nano-theme org-roam-ui org-preview-html xclip web-mode use-package typescript-mode treemacs-evil treemacs-all-the-icons tree-sitter-langs reverse-im projectile pdf-tools parrot org-superstar nov no-littering multi-term magit-todos lsp-ui lsp-ivy json-mode indent-guide helm-lsp gruvbox-theme go-mode fzf format-all flycheck-inline evil-tutor evil-multiedit evil-collection elfeed doom-themes doom-modeline diredfl dired-single dashboard dap-mode company-statistics company-quickhelp company-box company-anaconda blamer apheleia))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)) t)
 '(mode-line ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-active ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-inactive ((t (:family "Iosevka Aile" :height 0.95)))))
