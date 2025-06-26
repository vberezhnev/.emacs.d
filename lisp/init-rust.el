;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lsp-bridge setup for code completion

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  ;; :init
  ;; (global-lsp-bridge-mode)
  ;; (defun lsp-bridge-hook ()
  ;;   ;; Отключить company-mode и corfu в lsp-bridge
  ;;   (when (featurep 'corfu)
  ;;     (corfu-mode -1))
  ;;   (when (featurep 'company)
  ;;     (company-mode -1)))
  ;; :hook
  ;; ((emacs-lisp-mode    . lsp-bridge-mode)
  ;;  (rustic-mode        . lsp-bridge-mode)
  ;;  (typescript-mode    . lsp-bridge-mode)
  ;;  (typescript-ts-mode . lsp-bridge-mode)
  ;;  (dockerfile-mode    . lsp-bridge-mode))
  :bind (:map lsp-bridge-mode-map
              ("C-." . lsp-bridge-show-documentation)
              ("M-n" . lsp-bridge-diagnostic-jump-next)
              ("M-p" . lsp-bridge-diagnostic-jump-prev)
              ("M-." . lsp-bridge-find-def)
              ("M-," . lsp-bridge-find-def-return))
  :config
  (setq lsp-bridge-enable-hover-diagnostic t
	lsp-bridge-headerline-breadcrumb-enable nil
	lsp-headerline-breadcrumb-enable nil
	lsp-file-watch-threshold 3500 ;; Default: 1000
        acm-enable-capf t ;; Enable CAPF for non-LSP backends (e.g., Emacs Lisp)
        ;; lsp-bridge-enable-org-babel t
	;; lsp-bridge-enable-with-tramp t
	acm-enable-doc t
        lsp-bridge-enable-inlay-hint t
        ;; acm-enable-elisp t ;; Explicitly enable Emacs Lisp completion backend
        ;; acm-backend-elisp-candidate-min-length 0 ;; Trigger completion immediately
        lsp-bridge-headerline-breadcrumb-enable nil) ;; Disable breadcrumbs
  ;; Ensure acm is initialized for elisp
  (add-to-list 'acm-backend-capf-mode-list 'emacs-lisp-mode))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lsp-mode

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook ((lsp-mode . lsp-diagnostics-mode)
;;              (lsp-mode . lsp-enable-which-key-integration)
;;              ((tsx-ts-mode
;;                typescript-ts-mode
;;                js-ts-mode
;; 	       rustic-mode) . lsp-deferred))
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-file-watch-threshold 3500)
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.1)
;;   ;; This controls the overlays that display type and other hints inline. Enable
;;   ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
;;   ;; effect on open projects.
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   ;; (lsp-rust-analyzer-display-parameter-hints nil)
;;   ;; (lsp-rust-analyzer-display-reborrow-hints nil)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; lsp-booster

;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;   "Try to parse bytecode instead of json."
;;   (or
;;    (when (equal (following-char) ?#)
;;      (let ((bytecode (read (current-buffer))))
;;        (when (byte-code-function-p bytecode)
;;          (funcall bytecode))))
;;    (apply old-fn args)))
;; (advice-add (if (progn (require 'json)
;;                        (fboundp 'json-parse-buffer))
;;                 'json-parse-buffer
;;               'json-read)
;;             :around
;;             #'lsp-booster--advice-json-parse)

;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;   "Prepend emacs-lsp-booster command to lsp CMD."
;;   (let ((orig-result (funcall old-fn cmd test?)))
;;     (if (and (not test?)                             ;; for check lsp-server-present?
;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;              lsp-use-plists
;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;              (executable-find "emacs-lsp-booster"))
;;         (progn
;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
;;             (setcar orig-result command-from-exec-path))
;;           (message "Using emacs-lsp-booster for %s!" orig-result)
;;           (cons "emacs-lsp-booster" orig-result))
;;       orig-result)))
;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; flycheck for inline errors

(use-package flycheck
  :ensure t
  :hook  ((prog-mode . flycheck-mode) ;; Enable flycheck in all programming modes
	  (emacs-lisp-mode . flycheck-mode))
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error))) ;; Explicitly enable flycheck for Emacs Lisp

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; yasnippet for code snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode)
   (emacs-lisp-mode . yas-minor-mode))) ;; Ensure yasnippet in emacs-lisp-mode

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; docker and docker-compose

(use-package dockerfile-mode
  :ensure t
  ;; :hook (dockerfile-mode . lsp-mode)
  ) ;; Enable lsp-bridge for Dockerfiles

(use-package docker-compose-mode
  :ensure t)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic for Rust development

(use-package rustic
  :ensure t
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
  (setq eldoc-idle-delay 0.0)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-bridge-format-file nil t)) ;; Use lsp-bridge formatting

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; typescript

;; (use-package typescript-mode
;;   :ensure t
;;   :mode (("\\.ts\\'" . typescript-mode)
;;          ("\\.tsx\\'" . typescript-mode))
;;   :config
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")
;;   ;; :hook (typescript-mode . lsp-mode)
;;   )

;; (use-package treesit
;;       :mode (("\\.tsx\\'" . tsx-ts-mode)
;;              ("\\.js\\'"  . typescript-ts-mode)
;;              ("\\.mjs\\'" . typescript-ts-mode)
;;              ("\\.mts\\'" . typescript-ts-mode)
;;              ("\\.cjs\\'" . typescript-ts-mode)
;;              ("\\.ts\\'"  . typescript-ts-mode)
;;              ("\\.jsx\\'" . tsx-ts-mode)
;;              ("\\.json\\'" .  json-ts-mode)
;;              ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;              ("\\.prisma\\'" . prisma-ts-mode)
;;              ;; More modes defined here...
;;              )
;;       :preface
;;       (defun os/setup-install-grammars ()
;;         "Install Tree-sitter grammars if they are absent."
;;         (interactive)
;;         (dolist (grammar
;;                  '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                    (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                    (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
;;                    (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                    (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;; 		   (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;                    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
;;                    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;                    (make "https://github.com/alemuller/tree-sitter-make")
;;                    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                    (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                    (c "https://github.com/tree-sitter/tree-sitter-c")
;;                    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;                    (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                    (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
;;                    (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
;;           (add-to-list 'treesit-language-source-alist grammar)
;;           ;; Only install `grammar' if we don't already have it
;;           ;; installed. However, if you want to *update* a grammar then
;;           ;; this obviously prevents that from happening.
;;           (unless (treesit-language-available-p (car grammar))
;;             (treesit-install-language-grammar (car grammar)))))

;;       ;; Optional, but recommended. Tree-sitter enabled major modes are
;;       ;; distinct from their ordinary counterparts.
;;       ;;
;;       ;; You can remap major modes with `major-mode-remap-alist'. Note
;;       ;; that this does *not* extend to hooks! Make sure you migrate them
;;       ;; also
;;       (dolist (mapping
;;                '((python-mode . python-ts-mode)
;;                  (css-mode . css-ts-mode)
;;                  (typescript-mode . typescript-ts-mode)
;;                  (js-mode . typescript-ts-mode)
;;                  (js2-mode . typescript-ts-mode)
;;                  (c-mode . c-ts-mode)
;;                  (c++-mode . c++-ts-mode)
;;                  (c-or-c++-mode . c-or-c++-ts-mode)
;;                  (rustic-mode . rustic-ts-mode)
;;                  (bash-mode . bash-ts-mode)
;;                  (css-mode . css-ts-mode)
;;                  (json-mode . json-ts-mode)
;;                  (js-json-mode . json-ts-mode)
;;                  (sh-mode . bash-ts-mode)
;;                  (sh-base-mode . bash-ts-mode)))
;;         (add-to-list 'major-mode-remap-alist mapping))
;;       :config
;;       (os/setup-install-grammars))

(use-package format-all
  :ensure t)

(provide 'init-lsp)
;;; init-rust.el ends here
