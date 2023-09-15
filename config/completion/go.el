(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-go-install-save-hooks)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  (add-hook 'before-save-hook 'lsp-organize-imports t t))


;; (lsp-register-custom-settings
;; 		'(("gopls.completeUnimported" t t)
;; 		("gopls.staticcheck" t t)))
