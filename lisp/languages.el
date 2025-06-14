(use-package xclip
	:ensure t)

;;;;;;;;;;;;;;;;;;;;;;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0.1          
;;         company-minimum-prefix-length 1 
;;         company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase nil
;;         company-selection-wrap-around t 
;;         company-transformers '(company-sort-by-occurrence)) 
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;;   (define-key company-active-map (kbd "<return>") 'company-complete-selection)
;;   (setq company-frontends '(company-pseudo-tooltip-frontend))) ; Легкий фронтенд

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-BOX ;;;;;;;;;;;;;;;;;;;;;;;

;; Отключен company-box для снижения нагрузки
;; Раскомментируйте, если хотите попробовать с оптимизированными настройками
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-max-candidates 10   ; Минимальное количество кандидатов
;;         company-box-show-single-candidate t
;;         company-box-doc-delay 0.3))     ; Задержка для документации

;;;;;;;;;;;;;;;;;;;;;;; COMPANY-ORG-BLOCK ;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package company-org-block
  ;; :ensure t
  ;; :defer t
  ;; :custom
  ;; (company-org-block-edit-style 'auto)
  ;; :hook ((org-mode . (lambda ()
  ;;                      (setq-local company-backends '(company-org-block))
  ;;                      (company-mode +1)))))

;;;;;;;;;;;;;;;;;;;;;;; DIFF-HL ;;;;;;;;;;;;;;;;;;;;;;;

(use-package diff-hl
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;; FLYCHECK ;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :demand t
  :config
  ;; (add-hook 'lsp-bridge-mode-hook #'flycheck-mode)
  ;; (add-hook #'eglot-h #'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-popup-tip
	:ensure t
	:after flycheck
	:config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;; (use-package flycheck-rust
;; 	:ensure t
;; 	:after flycheck
;; 	:config
;; 	(add-hook 'rustic-mode-hook #'flycheck-rust-setup))

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
  :after (lsp-bridge flycheck)
  :defer t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-bridge-find-impl)
              ("M-?" . lsp-bridge-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-bridge-code-action)
              ("C-c C-c r" . lsp-bridge-rename)
              ("C-c C-c q" . lsp-bridge-restart-process)
              ("C-c C-c t" . rustic-cargo-test)
              ("C-c C-c r" . rustic-cargo-run)
              ("C-c C-c s" . lsp-bridge-diagnostic-list))
  :mode ("\\.rs$" . rustic-mode)
  :custom
  (rustic-lsp-client nil) ;; Отключите eglot
  :config
	(add-hook 'rustic-mode-hook #'lsp-bridge-mode)
  (add-hook 'rustic-mode-hook (lambda () (company-mode -1)))
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

;;;;;;;;;;;;;;;;;;;;;;; LSP-BRIDGE ;; ;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-bridge
;; 	:straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;; 	:init
;; 	(global-lsp-bridge-mode)
;;   :config
;;   ;; (global-lsp-bridge-mode)
;;   (setq lsp-bridge-enable-log t
;;         ;; lsp-bridge-rust-server "rust-analyzer"
;;         lsp-bridge-enable-inlay-hint t
;;         lsp-bridge-enable-diagnostics t
;;         lsp-bridge-enable-signature-help t
;;         ;; lsp-bridge-get-project-path-by-filepath
;;         ;; (lambda (filepath) (locate-dominating-file filepath "Cargo.toml"))
;; 				)
;;   :bind (:map lsp-bridge-mode-map
;;               ("M-j" . lsp-bridge-find-def)
;;               ("M-?" . lsp-bridge-find-references)
;;               ("C-c C-c a" . lsp-bridge-code-action)
;;               ("C-c C-c r" . lsp-bridge-rename)
;;               ("C-c C-c l" . lsp-bridge-diagnostic-list)))

;;;;;;;;;;;;;;;;;;;;;;; EGLOT ;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((rustic-mode rust-mode) . ("rust-analyzer"))
               ;; '((go-mode) . ("gopls"))
               '((typescript-mode typescriptreact-mode) . ("typescript-language-server" "--stdio"))
               ;; '((python-mode) . ("pylsp"))
               ;; '((c-mode c++-mode) . ("clangd"))
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

;; cargo install emacs-lsp-booster
(use-package eglot-booster
	:straight (:host github
									 :repo "jdtsmith/eglot-booster"
									 :branch "main" :files ("*.el" "out"))
	:after eglot
	:config	(eglot-booster-mode))

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

;;;;;;;;;;;;;;;;;;;;;;; NIX-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package nix-mode
	:ensure t)
	
;;;;;;;;;;;;;;;;;;;;;;; DOCKER-COMPOSE-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package docker-compose-mode
	:ensure t)

;;;;;;;;;;;;;;;;;;;;;;; DOTENV-MODE ;;;;;;;;;;;;;;;;;;;;;;;

(use-package dotenv-mode
	:ensure t)
