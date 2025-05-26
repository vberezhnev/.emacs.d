;;;;;;;;;;;;;;;;;;;;;;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-icon-margin 3)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-tng-mode)
  (setq company-tng-auto-configure nil)
  (with-eval-after-load 'company-tng
    (setq company-active-map company-tng-map)))

(setq company-frontends '(company-tng-frontend company-box-frontend))

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-BOX ;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icon-right-margin 1
        company-box-max-candidates 50
        company-box-show-single-candidate t
        company-box-doc-delay 0.0))

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-ORG-BLOCK ;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-org-block
  :ensure t
  :defer t
  :custom
  (company-org-block-edit-style 'auto)
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;;;;;;;;;;;;;;;;;;;;;;; DIFF-HL ;;;;;;;;;;;;;;;;;;;;;;;

(use-package diff-hl
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; FLYCHECK ;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :demand t
  :config
  (add-hook 'eglot-managed-mode-hook #'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;; SCRATCH ;;;;;;;;;;;;;;;;;;;;;;;

(use-package scratch
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; TYPESCRIPT-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

;;;;;;;;;;;;;;;;;;;;;;; RUSTIC ;;;;;;;;;;;;;;;;;;;;;;;

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
      (setq-local buffer-save-without-query t)))
  (use-package rust-playground
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;; GO-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 2)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends)
         '((company-go :with company-capf)))
    (eglot-ensure)
    (company-mode))
  (add-hook 'go-mode-hook #'my-go-mode-hook)
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t)
              (add-hook 'before-save-hook
                        (lambda () (when (eglot--server-capable :codeActionProvider)
                                     (eglot-code-actions nil "source.organizeImports")))
                        nil t))))

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-GO ;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-go
  :ensure t
  :after (company go-mode))

;;;;;;;;;;;;;;;;;;;;;;; EGLOT ;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((rustic-mode rust-mode) . ("rust-analyzer"))
               '((go-mode) . ("gopls"))
               '((typescript-mode typescriptreact-mode) . ("typescript-language-server" "--stdio"))
               '((python-mode) . ("pylsp"))
               '((c-mode c++-mode) . ("clangd"))
               '((js-mode) . ("typescript-language-server" "--stdio")))
  (setq eglot-autoshutdown t
        eglot-sync-connect 0
        eglot-confirm-server-initiated-edits nil
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.2)
  (setq eldoc-idle-delay 0.0
        eldoc-echo-area-use-multiline-p t
        eldoc-documentation-strategy 'eldoc-documentation-compose)
  (defun my/eglot-format-on-save ()
    (when (eglot-managed-p)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'my/eglot-format-on-save)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (eglot--server-capable :inlayHintProvider)
                (eglot-inlay-hints-mode))))
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format-buffer)
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . xref-find-definitions)
              ("C-c l ?" . xref-find-references)
              ("C-c l e" . flycheck-list-errors)
              ("C-c l q" . eglot-shutdown)
              ("M-j" . eglot-code-action-quickfix)))

;;;;;;;;;;;;;;;;;;;;;;; HELM-LSP ;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-lsp
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; FORMAT-ALL ;;;;;;;;;;;;;;;;;;;;;;;

(use-package format-all
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

;;;;;;;;;;;;;;;;;;;;;;; DOCKERFILE-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; JSON-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; PROTOBUF-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package protobuf-mode
  :ensure t)


(use-package nix-mode
	:ensure t)
