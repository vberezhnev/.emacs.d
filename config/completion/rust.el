
(use-package rust-playground  )

(use-package rust-mode
  :if (executable-find "rustc"))

(use-package cargo
  :if (executable-find "cargo")
  :after rust-mode
  :bind (:map cargo-minor-mode-map
              ("C-c C-t" . cargo-process-test)
              ("C-c C-b" . cargo-process-build)
              ("C-c C-c" . cargo-process-run))
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(add-hook 'rust-mode-hook 'lsp-deferred)
