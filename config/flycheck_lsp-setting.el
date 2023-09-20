;;________________________________________________________________
;;;    LSP & Completion
;;________________________________________________________________
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-tex-server 'digestif)
  (setq lsp-idle-delay 0.2)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . lsp-mode)) ;; auto-enable for .js/.jsx files
(add-to-list 'auto-mode-alist '("\\.tsx?$" . lsp-mode)) ;; auto-enable for .ts/.tsx files
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode         . lsp)
         (rust-mode       . lsp)
         ;; (web-mode        . lsp)
         (js-mode         . lsp)
         (typescript-mode . lsp)
         (LaTeX-mode      . lsp)
         ;; if you want which-key integration
         (lsp-mode        . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-enable-snippet t))

(add-hook 'js-mode-hook #'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp-mode) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-mode) ;; for js3-mode support

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-delay 0.01)
  (setq lsp-ui-sideline-show-code-actions t))

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
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
;; (setq completion-cycle-threshold 3)

(use-package tree-sitter
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;________________________________________________________________
;;;    Golang
;;________________________________________________________________

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

;;________________________________________________________________
;;;    JavaScript / TypeScript
;;________________________________________________________________

(use-package import-js)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

(use-package tide
  :after (company flycheck)
  :hook ((typescript-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq tide-format-options '(:tabSize 2 :indentSize 2 ))
;; TSX with treesitter
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(use-package json-mode   :defer 20
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
(add-hook 'rust-mode-hook 'lsp-deferred)

;;________________________________________________________________
;;;    Company
;;________________________________________________________________
(use-package company
  :config
  (setq company-minimum-prefix-length 3
        company-idle-delay 0
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case nil
        company-etags-ignore-case nil
        company-etags-file-name-prefix ""
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '((company-capf :with company-yasnippet)
                           company-files
                           (company-dabbrev-code company-gtags company-keywords)
                           company-dabbrev))
  (setq lsp-prefer-capf t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)
  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))
  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

(use-package company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package company-auctex
	:after (latex)
  :config
  ;; Set up default LaTeX preview configuration
  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-preview-latex-default-process 'imagemagick) ; or 'dvisvgm
  (setq org-preview-latex-process-alist
        '((imagemagick :programs ("latex" "convert")
                       :description "imagemagick"
                       :message "You need to install the programs: latex and imagemagick."
                       :image-input-type "pdf"
                       :image-output-type "png"
                       :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
          (dvisvgm :programs ("latex" "dvisvgm")
                   :description "dvisvgm"
                   :message "You need to install the programs: latex and dvisvgm."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))
  ;; Enable inline LaTeX previews in org-mode
  (add-hook 'org-mode-hook 'org-toggle-latex-fragment)
  ;; Display images in org-mode buffers
  (setq org-image-actual-width nil) ; adjust to your liking
  (setq org-startup-with-inline-images t)
  (use-package ac-math))
(company-auctex-init)

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

(provide 'lsp-setting)
