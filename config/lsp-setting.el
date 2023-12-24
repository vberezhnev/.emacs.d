;;________________________________________________________________
;;;    LSP & Completion
;;________________________________________________________________
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :straight t
  :hook ((go-mode         . lsp)
         (rust-mode       . lsp)
         (js-mode         . lsp)
	 (solidity-mode   . lsp)
         (typescript-mode . lsp)
         (LaTeX-mode      . lsp)
         (lsp-mode        . lsp-enable-which-key-integration))
  ;; :config
  ;; (delete 'lsp-terraform lsp-client-packages)
  ;; :commands lsp
  :custom
  (lsp-enable-snippet t))

;; optionally
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   ;; lsp-ui-doc-max-width 100         ; 150 (default) is too wide
   ;; lsp-ui-doc-delay 0.5           ; 0.2 (default) is too naggy
   lsp-ui-doc-show-with-mouse t  ; don't disappear on mouseover
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header t))

;; lsp-ui-sideline-delay 0.03
;; lsp-ui-sideline-ignore-duplicate t
;; lsp-ui-sideline-show-code-actions t
;; lsp-ui-sideline-update-mode 'line
;; lsp-ui-sideline-show-diagnostics t
;; lsp-ui-sideline-show-hover t
;; lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default

;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)
;; Symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

;; Need for LSP
(use-package sideline
  :init
  (setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        sideline-display-backend-name t))      ; display the backend name

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)

;;________________________________________________________________
;;;    Solidity
;;________________________________________________________________
(use-package solidity-mode)
(use-package solidity-flycheck)
(use-package company-solidity)

;;________________________________________________________________
;;;    Golang
;;________________________________________________________________
(use-package go-mode)
(add-hook 'go-mode-hook 'lsp-go-install-save-hooks)

;; (add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()

;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))

;; (use-package gofmt-tag
;;   :config
;;   (add-hook 'go-mode-hook 'gofmt-tag-mode))

;;________________________________________________________________
;;;    JavaScript / TypeScript
;;________________________________________________________________

(use-package js-import)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

;; (use-package tide
;;   :after (company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (tsx-ts-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))
;; (setq tide-format-options '(:tabSize 2 :indentSize 2 ))

;; TSX with treesitter
;; (add-hook 'tsx-ts-mode-hook 'setup-tide-mode)
;; (add-hook 'js2-mode-hook 'setup-tide-mode)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook 'setup-tide-mode)

(use-package json-mode
  :defer 20
  :custom
  (json-reformat:indent-width 2)
  :mode (("\\.bowerrc$"     . json-mode)
	 ("\\.jshintrc$"    . json-mode)
	 ("\\.json_schema$" . json-mode)
	 ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
		  :map json-mode-map
		  ("C-c <tab>" . json-mode-beautify)))

;;________________________________________________________________
;;;    Rust
;;________________________________________________________________
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
;; (add-hook 'rust-mode-hook 'lsp-deferred)

;;________________________________________________________________
;;;    Company
;;________________________________________________________________
;; (use-package company
;;   :init
;;   (setq company-minimum-prefix-length 2
;;         company-tooltip-limit 14
;;         company-tooltip-align-annotations t
;;         company-require-match 'never
;;         company-global-modes
;;         '(not erc-mode
;;               circe-mode
;;               message-mode
;;               help-mode
;;               gud-mode
;;               vterm-mode)
;;         company-frontends
;;         '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
;;           company-echo-metadata-frontend)  ; show selected candidate docs in echo area

;;         ;; Buffer-local backends will be computed when loading a major mode, so
;;         ;; only specify a global default here.
;;         company-backends '(company-capf)

;;         ;; These auto-complete the current selection when
;;         ;; `company-auto-commit-chars' is typed. This is too magical. We
;;         ;; already have the much more explicit RET and TAB.
;;         company-auto-commit nil

;;         ;; Only search the current buffer for `company-dabbrev' (a backend that
;;         ;; suggests text your open buffers). This prevents Company from causing
;;         ;; lag once you have a lot of buffers open.
;;         company-dabbrev-other-buffers nil
;;         ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
;;         ;; domain-specific words with particular casing.
;;         company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase nil)
;;   :custom
;;   (company-idle-delay 0.2) ;; how long to wait until popup
;;   ;; (company-begin-commands nil) ;; uncomment to disable popup
;;   :bind
;;   (:map company-active-map
;; 	("C-n". company-select-next)
;; 	("C-p". company-select-previous)
;; 	("M-<". company-select-first)
;; 	("M->". company-select-last)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 30))

(use-package company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; (use-package company-auctex
;;   :after (latex)
;;   :config
;;   ;; Set up default LaTeX preview configuration
;;   ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
;;   (setq org-latex-create-formula-image-program 'imagemagick)
;;   (setq org-preview-latex-default-process 'imagemagick) ; or 'dvisvgm
;;   (setq org-preview-latex-process-alist
;;         '((imagemagick :programs ("latex" "convert")
;;                        :description "imagemagick"
;;                        :message "You need to install the programs: latex and imagemagick."
;;                        :image-input-type "pdf"
;;                        :image-output-type "png"
;;                        :image-size-adjust (1.0 . 1.0)
;;                        :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
;;                        :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
;;           (dvisvgm :programs ("latex" "dvisvgm")
;;                    :description "dvisvgm"
;;                    :message "You need to install the programs: latex and dvisvgm."
;;                    :image-input-type "xdv"
;;                    :image-output-type "svg"
;;                    :image-size-adjust (1.7 . 1.5)
;;                    :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
;;                    :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))
;;   ;; Enable inline LaTeX previews in org-mode
;;   (add-hook 'org-mode-hook 'org-toggle-latex-fragment)
;;   ;; Display images in org-mode buffers
;;   (setq org-image-actual-width nil) ; adjust to your liking
;;   (setq org-startup-with-inline-images t)
;;   (use-package ac-math))
;; (company-auctex-init)			;

;;________________________________________________________________
;;;    Corfu
;;________________________________________________________________
;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   :hook ((prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `global-corfu-modes'.
;;   :init
;;   (global-corfu-mode))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   (setq completion-cycle-threshold 3)

;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   (setq read-extended-command-predicate
;;         #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (setq tab-always-indent 'complete))

;; (provide 'lsp-setting)
