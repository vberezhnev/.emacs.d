(use-package company
    :ensure t
    :config
		(setq company-idle-delay 0
					company-minimum-prefix-length 1)

	;; Go - lsp-mode
	;; Set up before-save hooks to format buffer and add/delete imports.
	;; (defun lsp-go-install-save-hooks ()
	;; 	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	;; 	(add-hook 'before-save-hook #'lsp-organize-imports t t))
	;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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
  :ensure t
	:demand t)

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
	:after (eglot flycheck)
	:defer t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
							("C-c C-c t" . rustic-cargo-test)
							("C-c C-c r" . rustic-cargo-run)
							("C-c C-c s" . lsp-rust-analyzer-status))
 	:mode ("\\.rs$" . rustic-mode)
	:custom
	(rustic-lsp-client 'eglot)
	(rustic-lsp-server 'rust-analyzer)
  :config
	(add-hook 'rustic-mode-hook #'eglot-ensure)
	
	(defun rk/rustic-mode-hook ()
		(when buffer-file-name
			(setq-local buffer-save-without-query t))
		((and )dd-hook 'before-save-hook 'lsp-format-buffer nil t))
	
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

;; (use-package go-errcheck
;;   :after go-mode
;; 	:ensure t
;;   :straight t)

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))

;;   ;; (setq-default eglot-workspace-configuration
;;   ;;               '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

;;   :hook
;;   ((rustic-mode . eglot-ensure)))

;; (use-package lsp-mode
;;   :ensure t
;; 	:demand t
;;   :commands (lsp lsp-deferred)
;;   :bind (:map lsp-mode-map
;;               ("C-c f" . lsp-format-buffer))
;;   :hook ((go-mode         . lsp-deferred)
;;          (rustic-mode       . lsp-deferred)
;;          (lisp            . lsp)
;;          (python-mode     . lsp-deferred)
;;          (c-mode          . lsp-deferred)
;;          (c++-mode        . lsp-deferred)
;;          (js-mode         . lsp-deferred)
;;          (typescript-mode . lsp-deferred)
;;          (lsp-mode        . lsp-enable-which-key-integration))
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0)
;;   ;; enable / disable the hints as you prefer:
;;   (lsp-inlay-hint-enable t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints t)
;;   ;; (lsp-rust-analyzer-display-reborrow-hints t)

;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil
;; 	      lsp-signature-render-documentation nil
;; 	      lsp-enable-snippet t ;; nil
;; 	      lsp-lens-enable t ;; nil
;; 	      lsp-diagnostic-provider :none)
;; 	(with-eval-after-load 'lsp-mode
;; 	(set-face-attribute 'lsp-face-highlight-read nil :underline nil)))

(use-package helm-lsp
  :ensure t)

;; (use-package lsp-ui
;;   :ensure t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :commands lsp-ui-mode
;;   :config
;;   (setq
;;    lsp-inlay-hints-mode t
;;    lsp-ui-doc-enable t
;;    lsp-ui-doc-max-height 8
;;    lsp-ui-doc-max-width 130         ; 150 (default) is too wide
;;    lsp-ui-doc-delay 0.0           ; 0.2 (default) is too naggy
;;    lsp-ui-doc-show-with-mouse t  ; don't disappear on mouseover
;;    ;; lsp-ui-doc-show-with-cursor t
;;    lsp-ui-doc-border (face-foreground 'default)
;;    lsp-ui-doc-position 'at-point
;;    lsp-ui-doc-include-signature t
;;    lsp-ui-doc-header t))

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
