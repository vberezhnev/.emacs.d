(setq gui-p         (display-graphic-p)
      cli-p         (not gui-p)
      android-p     (getenv "ANDROID_ROOT")
      linux-p       (and (eq system-type 'gnu/linux) (not android-p))
      windows-p     (eq system-type 'windows-nt)
      workstation-p (member (system-name)
                            '("berezhnev")))

;; (if (file-exists-p custom-file)
;;     (load custom-file))

(electric-pair-mode)
(global-auto-revert-mode t)

(define-coding-system-alias 'UTF-8 'utf-8)

(setq custom-file (concat user-emacs-directory "custom.el")
      mouse-yank-at-point t
 
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file

			scroll-step 1
			scroll-margin 5         			
			scroll-conservatively 101

			initial-scratch-message ";; Do you even lisp, bro? ಠ_ಠ\n\n\n"
			backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
			auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
			auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
			undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
			kill-buffer-query-functions nil

			evil-shift-width 2
			tab-width 2)

(setq flycheck-highlighting-mode 'lines)

(save-place-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-hook 'auto-package-update-after-hook
          (lambda ()
            (load-library "yasnippet-snippets.el")))
(toggle-frame-fullscreen)

(xterm-mouse-mode t)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun frostyx/set-default-font (size)
  (set-face-attribute
   'default nil
   :family "Iosevka"
   ;;:foundry "ADBO"
   :height 150 ;;size
   :weight 'regular ;;'normal
   :width 'normal
   :slant 'normal))

(frostyx/set-default-font 90)

(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(defun hook-tab-width ()
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq python-indent-offset 2))
(add-hook 'prog-mode-hook #'hook-tab-width)

;;;;;;;;;;;;;;;;;;;;;;;; END OF INITIALS ;;;;;;;;;;;;;;;;;;;;;;;

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package system-packages
  :ensure t)

(defun frostyx/guix (&key install)
  (let ((system-packages-package-manager 'guix)
        (system-packages-use-sudo nil))
    (or (frostyx/rpm-query install)
 	  (system-packages-install install))))

(defun frostyx/rpm-query (pack)
  (equal 0 (shell-command
            (concat "rpm -q " pack))))

(setq system-packages-package-manager 'guix)
(setq system-packages-use-sudo t)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
	(setq display-line-numbers 'relative
        display-line-numbers-width 3))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

	(setq display-line-numbers 'relative)

(use-package no-littering
  :ensure t)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 365
         auto-package-update-prompt-before-update nil
         auto-package-update-hide-results t)
   (auto-package-update-at-time "04:00"))

;;;;;;;;;;;;;;;;;;;;;; EVIL ;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/evil.el")

;;;;;;;;;;;;;;;;;;;;; ORG-MODE ;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/org-mode.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;; THEMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :if window-system
  :ensure t
  :defer nil
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (if (display-graphic-p)
      (progn
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package theme-changer
  :ensure nil
  :demand nil
  :straight (:host github :repo "hadronzoo/theme-changer" :branch "master")
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88))

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-themes '((doom-xcode) (doom-one-light))
			auto-dark-polling-interval-seconds 0
			auto-dark-allow-osascript nil
			auto-dark-allow-powershell nil)
  (auto-dark-mode t))

;;;;;;;;;;;;;;;;;;;;;;;; GIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
	:ensure t)

(use-package git-gutter
	:ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
	:ensure t
	:config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(evil-leader/set-key
  "ga" 'magit-stage-file
  "gc" 'magit-commit  ;; Maybe magit-commit-create
  "gp" 'magit-push-current) ;; @TODO still asks for something, use more specific function

;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :after all-the-icons
  :ensure t
  :defer nil
  :config
  (setq dashboard-banner-logo-title "Welcome back, Darling!"
        dashboard-startup-banner "~/.emacs.d/images/Emacs-logo.svg"
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        ;; dashboard-startupify-list     nil
        dashboard-set-heading-icons t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-set-file-icons    t)
  (setq dashboard-items '((recents  . 12)
                          ;; (agenda   . 6)
                          (projects . 6))))

(dashboard-setup-startup-hook)

(use-package fontawesome
  ;; :disabled nil
  :ensure t)

(set-fontset-font t 'unicode "FontAwesome" nil 'prepend)

(use-package all-the-icons
  :defer t
  :ensure t)

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
	:config
	(setq doom-modeline-height 22
			all-the-icons-scale-factor 1.0
			doom-modeline-icon nil
			doom-modeline-buffer-file-name-style 'buffer-name
			doom-modeline-buffer-encoding nil
			doom-modeline-env-version nil
			doom-modeline-mode-alist '()))

(use-package nerd-icons
  :ensure)

(use-package hide-mode-line
  :ensure t
  :config
  (evil-leader/set-key
    "h" 'global-hide-mode-line-mode))

(use-package olivetti
  :ensure t
  :defer t
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)))
(setq olivetti-body-width 150)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-fuzzy-match t)
	(setq helm-move-to-line-cycle-in-source nil)

  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)

  (define-key helm-map [escape] 'helm-keyboard-quit))

(use-package helm-posframe
  :ensure t
  :disabled
  :config
  (helm-posframe-enable)
  (setq helm-posframe-poshandler
        #'posframe-poshandler-frame-center)
  (setq helm-posframe-width 200)
  (setq helm-posframe-height 600)
  (setq helm-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package rainbow-mode
  :ensure t
  :config
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGES ;;;;;;;;;;;;;;;
(use-package company
    :ensure t
    :config
;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode))

(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)

(setq company-icon-margin 3)

(add-hook 'after-init-hook 'company-tng-mode)

(setq company-tng-auto-configure nil)
(with-eval-after-load 'company-tng
  (setq company-active-map company-tng-map))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode)

  :config
  (setq company-box-icon-right-margin 1))

(setq company-frontends '(company-tng-frontend company-box-frontend))

(use-package company-org-block
  :ensure t
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package diff-hl
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(setq yas-snippet-dirs
      (list (expand-file-name "snippets/" user-emacs-directory)
            yasnippet-snippets-dir))
(yas-reload-all)

(use-package scratch
  :ensure t)

;; (general-vmap
;;   :keymaps 'lisp-interaction-mode-map
;;   "<tab>" #'indent-region)

;; (use-package package-lint
;;   :ensure t)

(use-package json-mode
  :ensure t)

(use-package protobuf-mode
	:ensure t)

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
	(defun rk/rustic-mode-hook ()
		(when buffer-file-name
			(setq-local buffer-save-without-query t))
		(add-hook 'before-save-hook 'lsp-format-buffer nil t))

	(use-package rust-playground
			:ensure t))

(use-package go-mode
  :straight t
	:ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 2)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :after (company go-mode)
	:ensure t
  :straight t)

(use-package go-errcheck
  :after go-mode
	:ensure t
  :straight t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-buffer))
  :hook (;;(go-mode         . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (lisp            . lsp)
         (python-mode     . lsp-deferred)
         (c-mode          . lsp-deferred)
         ;; (c++-mode        . lsp-deferred)
         (js-mode         . lsp-deferred)
         ;; (solidity-mode   . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  ;; (lsp-rust-analyzer-display-reborrow-hints t)

  :config
  (setq lsp-headerline-breadcrumb-enable nil
	      lsp-signature-render-documentation nil
	      lsp-enable-snippet t ;; nil
	      lsp-lens-enable t ;; nil
	      lsp-diagnostic-provider :none)
	(with-eval-after-load 'lsp-mode
	(set-face-attribute 'lsp-face-highlight-read nil :underline nil)))

(use-package helm-lsp
  :ensure t)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq
   lsp-inlay-hints-mode t
   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 130         ; 150 (default) is too wide
   lsp-ui-doc-delay 0.0           ; 0.2 (default) is too naggy
   lsp-ui-doc-show-with-mouse t  ; don't disappear on mouseover
   ;; lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header t))

(use-package dockerfile-mode
  :ensure t)

(use-package format-all
  :ensure t
  :preface
  ;; (defun ian/format-code ()
  ;;   "Auto-format whole buffer."
  ;;   (interactive)
  ;;   (if (derived-mode-p 'prolog-mode)
  ;;       (prolog-indent-buffer)
  ;;     (format-all-buffer)))
  :config
  ;; (global-set-key (kbd "M-F") 'ian/format-code)
  (global-set-key (kbd "C-c C-f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

;; (use-package direnv
;; 	:ensure t
;; 	:config
;; 	(direnv-mode))

(use-package gptel
	:disabled t
  :ensure t
  :init
  (setq gptel-api-key (getenv "AIML_API"))
  (setq gptel-max-tokens 8024)
  :config
  (setq gptel-model 'gpt-4o
        gptel-backend
        (gptel-make-openai "AIMLAPI"
          :host "api.aimlapi.com"
          :endpoint "/chat/completions"
          :stream t
          :key gptel-api-key
          :models '(gpt-4o
                    gpt-4o-2024-08-06
                    gpt-4-turbo
                    chatgpt-4o-latest)))
  :bind (("M-s M-d" . gptel-context-add)
         ("M-s M-f" . gptel-add-file)
         ("M-s M-a" . gptel-menu)
         ("M-s M-r" . gptel--regenerate)
         ("M-s M-e" . gptel-rewrite)
         ("M-s M-s" . gptel)))

(use-package whisper
  :load-path "~/.emacs.d/lisp/whisper.el"
  :bind ("M-s M-t" . whisper-run)
  :config
  (setq whisper-install-directory "/tmp/"
        whisper-model "base"
        ;; whisper-model "base"
        whisper-language "ru"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)
        whisper-enable-speed-up nil
        whisper-recording-timeout 300))

