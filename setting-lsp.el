;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; LSP ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(lsp-treemacs-sync-mode 1)

(add-hook 'web-mode-hook  'emmet-mode)

(helm-mode)

(use-package react-snippets)


;; JSX syntax highlighting
;;add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;///////////;;
;;/ COMPANY /;;
;;///////////;;

;; Install company
(use-package company
  :ensure t
  :config (add-hook 'prog-mode-hook 'company-mode)
  (global-set-key (kbd "M-i") 'company-complete))
(use-package company-anaconda
  :defer
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;;------COMPANY------;;

;;////////////;;
;;/ LSP MODE /;;
;;////////////;;

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         (javascript-mode . lsp)
         (typescript-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; Symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

;;------LSP MODE------;;

;;//////////;;
;;/ VUE.JS /;;
;;//////////;;

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

(setq vue-mode-packages
			'(vue-mode))

(setq vue-mode-excluded-packages '())
(defun vue-mode/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode))

;;------VUE.JS------;;

;;//////////////;;
;;/ TYPESCRIPT /;;
;;//////////////;;

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;;------TYPESCRIPT------;;

;; ;;//////////////;;
;; ;;/ JAVASCRIPT /;;
;; ;;//////////////;;

;; (use-package javascript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
;;   ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "JavaScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . javascript-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(javascript-mode . jsx)))

;; ;;------JAVASCRIPT------;;

;;//////////;;
;;/ Golang /;;
;;//////////;;

(use-package 'company-go
	:load-path "~/.emacs.d/local-packages/company-go")

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

;;------Golang------;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
;;(use-package tsi
;;  :after tree-sitter
;;  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;  ;; define autoload definitions which when actually invoked will cause package to be loaded
;;  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;  :init
;;  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))


;;(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
