;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;; (use-package corfu
;;   :straight (:host github :repo "minad/corfu" :branch "main")
;;   :demand t
;;   :custom
;;   (corfu-auto t)
;;   :init
;;   (global-corfu-mode))

;; (use-package orderless
;;   :straight (:host github :repo "oantolin/orderless" :branch "master")
;;   :demand t
;;   :config
;;   (setq completion-styles '(orderless partial-completion)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (use-package yasnippet
;;   :straight (:host github :repo "joaotavora/yasnippet" :branch "master")
;;   ;; :diminish yas-minor-mode
;;   :hook (prog-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :straight t
;;   :defer t
;;   :after yasnippet)

;; (use-package corfu
;;   :straight t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-auto-prefix 1)
;;   (corfu-separator ?\s)          ;; Orderless field separator
;;   (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
;;   (corfu-preview-current nil)    ;; Disable current candidate preview
;;   (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :config
;;   (setq completion-styles '(orderless)
;; 	completion-category-defaults nil
;; 	completion-category-overrides '((file (styles . (partial-completion)))))
;;   (setq tab-always-indent 'complete)

;;   :init
;;   (global-corfu-mode))

;; Add extensions
;; (use-package cape
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-history)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-tex)
;;   (add-to-list 'completion-at-point-functions #'cape-sgml)
;;   (add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-ispell)
;;   (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   (add-to-list 'completion-at-point-functions #'cape-line))

;; (use-package nerd-icons-corfu
;;   :straight t
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package kind-icon
;;   :straight t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   ;; (kind-icon-blend-background nil)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eglot
  :straight (:type built-in)
  :defer t
  ;; :init
  ;; (lsp-mode nil)
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq eglot-inlay-hints-mode t)
  (push :documentHighlightProvider eglot-ignored-server-capabilities))

(use-package eglot-booster
  :straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; (use-package eldoc-box
;;   :straight t
;;   :hook
;;   (rustic-mode . eldoc-box-hover-mode)
;;   :config
;;   ;; (setq eldoc-box-)
;;   (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

;; (use-package flycheck-hl-todo
;;   :straight t)

(use-package flycheck
  :straight t
  ;; :commands (flycheck-mode flycheck-next-error flycheck-previous-error)
  :hook ((prog-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode))
  :bind (:map flycheck-mode-map
              ;; ("M-n" . flycheck-next-error)
              ;; ("M-p" . flycheck-previous-error)
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-previous-error)
	      ))

(use-package flycheck-pos-tip
  :straight (:host github :repo "flycheck/flycheck-pos-tip" :branch "master")
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; Yasnippet: Load for programming and text modes
;; (use-package yasnippet
;;   :straight t
;;   ;; :commands (yas-minor-mode yas-reload-all)
;;   :hook ((prog-mode . yas-minor-mode)
;;          (text-mode . yas-minor-mode)
;;          (emacs-lisp-mode . yas-minor-mode))
;;   :config
;;   (yas-reload-all))

;; Dockerfile-mode: Load for Dockerfile files
(use-package dockerfile-mode
  :straight t
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :defer t)

;; Docker-compose-mode: Load for docker-compose files
(use-package docker-compose-mode
  :straight t
  :mode ("\\docker-compose.*\\.ya?ml\\'" . docker-compose-mode)
  :defer t)

;; Rustic: Load for Rust files
(use-package rustic
  :straight t
  ;; :mode ("\\.rs\\'" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-bridge-code-action)
              ("C-c C-c r" . lsp-bridge-rename)
              ("C-c C-c q" . lsp-bridge-restart)
              ("C-c C-c Q" . lsp-bridge-shutdown)
              ("C-c C-c s" . lsp-bridge-show-workspace-configuration)
              ("C-c C-c e" . lsp-bridge-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-bridge-inlay-hint-toggle))
  :config
  (setq rustic-lsp-client 'eglot)
  (setq eldoc-idle-delay 0.0)
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

;; (defun rk/rustic-mode-hook ()
;;   "Custom hook for rustic-mode."
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t))
;;   (add-hook 'before-save-hook 'lsp-bridge-format-file nil t))

;; (use-package js-mode
;;   :straight t
;;   :hook ((js-mode . subword-mode)
;;          (js-mode . electric-pair-mode)
;;          (js-mode . eglot-ensure)
;;          (js-mode . completion-preview-mode)))

;; Typescript-mode: Load for TypeScript/TSX files
(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :after (company flycheck)
  :config
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX"))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

(use-package eglot
  :straight (:type built-in)
  :hook ((typescript-mode typescript-ts-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-ts-mode) .
                 ("typescript-language-server" "--stdio"))))

;; Format-all: Load on demand
(use-package format-all
  :straight t
  :bind (("C-c f" . format-all-buffer)
	 ("C-c C-f" . eglot-format))
  :commands (format-all-buffer format-all-mode)
  :defer t)

(use-package tree-sitter
  :straight t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :straight t
;;   :after tree-sitter)

(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

;; (add-to-list 'eglot-server-programs
;;              '((typescript-mode) "typescript-language-server" "--stdio"))

(provide 'init-rust)
;;; init-rust.el ends here
