(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
				("ORG"		. "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"	. 20)
				("MELPA"        . 15)
				("MELPA Stable" . 10)
        ("ORG"          . 5))
      )

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
 '(before-save-hook '(parrot-start-animation format-all-buffer))
 '(custom-safe-themes
	 '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "" default))
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-file-name-style 'auto)
 '(doom-modeline-buffer-name nil)
 '(doom-modeline-buffer-state-icon t)
 '(doom-modeline-height 30)
 '(doom-modeline-highlight-modified-buffer-name nil)
 '(doom-modeline-icon t)
 '(doom-modeline-project-detection 'projectile)
 '(global-wakatime-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))
 '(org-agenda-files '("~/Org/agenda.org"))
 '(package-selected-packages
	 '(lorem-ipsum neotree zygospore xwwp xwidgets-reuse xwidgete xml+ xclip which-key web-mode use-package treemacs-evil treemacs-all-the-icons tree-sitter-langs tide telega saveplace-pdf-view rust-playground rust-auto-use reverse-im ranger rainbow-delimiters racer projectile pdf-view-restore pbcopy parrot org-superstar org-modern org-caldav org-alert nov names multi-vterm multi-term lsp-ui json-mode js2-mode indent-guide import-js helm gruvbox-theme graphql go-mode git-gutter-fringe general fzf format-all flycheck-rust flycheck-inline evil-collection emojify elfeed-dashboard doom-themes doom-modeline dmenu djvu dired-subtree dashboard dap-mode company-statistics company-quickhelp company-anaconda cl-libify cargo calibredb atom-one-dark-theme apheleia all-the-icons-dired))
 '(warning-suppress-log-types '(((org-roam)) ((org-roam)) (comp)))
 '(warning-suppress-types '(((org-roam)) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)) t)
 '(doom-modeline-time ((t (:inherit (mode-line-buffer-id bold) :box (:line-width (2 . 2) :color "dim gray" :style flat-button))))))
(put 'dired-find-alternate-file 'disabled nil)
