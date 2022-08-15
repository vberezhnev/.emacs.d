;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; LSP ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(lsp-treemacs-sync-mode 1)

(add-hook 'web-mode-hook  'emmet-mode)

(helm-mode)

;;///////////;;
;;/ COMPANY /;;
;;///////////;;

; Install company
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

; Company language package for PHP
(use-package company-php
  :defer
  :after company)

; Just as an example, aso Ruby:
;(use-package robe ;; company-robe is a Ruby mode
;  :ensure t
;  :after company
;  :config (add-to-list 'company-backends 'company-robe)
;          (add-hook 'ruby-mode-hook 'robe-mode))


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
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; for completions
;(use-package company-lsp
;  :after lsp-mode
;  :config (push 'company-lsp company-backends))

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

; Symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

