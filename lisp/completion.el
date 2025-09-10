;;; completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; Vertico: Load after init
(use-package vertico
  :straight t
  :commands (vertico-mode vertico-next vertico-previous vertico-directory-delete-char)
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("DEL" . vertico-directory-delete-char))
  :init
  (require 'recentf) ;; Needed for consult-recent-file and consult-buffer
  (require 'minibuffer) ;; Needed for minibuffer completions
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  :config
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

;; Orderless: Load after init
(use-package orderless
  :straight t
  :commands (orderless-filter)
  :hook (after-init . (lambda () (setq completion-styles '(orderless basic))))
  :init
  (setq orderless-affix-dispatch-alist
        '((?! . orderless-without-literal)
          (?& . orderless-annotation)
          (?% . char-fold-to-regexp)
          (?` . orderless-initialism)
          (?= . orderless-literal)
          (?^ . orderless-literal-prefix)
          (?~ . orderless-flex))
        completion-category-defaults nil
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  :config
  (set-face-attribute 'completions-first-difference nil :inherit nil)
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

;; Consult: Load for specific commands
(use-package consult
  :straight t
  :commands (consult-bookmark consult-goto-line consult-imenu consult-locate
             consult-theme consult-recent-file consult-buffer
             consult-buffer-other-window consult-buffer-other-frame consult-yank-pop)
  :bind (([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop))
  :config
  (advice-add #'consult-recent-file :before (lambda (&rest _) (recentf-mode +1)))
  (advice-add #'consult-buffer :before (lambda (&rest _) (recentf-mode +1)))
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind") "fdfind" "fd")
          "--color=never"
          "--full-path --absolute-path"
          "--hidden --exclude .git"))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any)))

;; Consult-dir: Load for directory navigation
(use-package consult-dir
  :straight t
  :commands (consult-dir consult-dir-jump-file)
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

;; Consult-flycheck: Load after consult and flycheck
(use-package consult-flycheck
  :straight t
  :after (consult flycheck)
  :commands (consult-flycheck)
  :defer t)

;; Consult-yasnippet: Load after consult and yasnippet
(use-package consult-yasnippet
  :straight t
  :after (consult yasnippet)
  :commands (consult-yasnippet)
  :bind ([remap yas-insert-snippet] . consult-yasnippet))

;; Embark: Load for action commands
(use-package embark
  :straight t
  :commands (embark-act embark-export embark-collect)
  :bind (("C-;" . embark-act)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

;; Embark-consult: Load after embark and consult
(use-package embark-consult
  :straight t
  :after (embark consult)
  :defer t)

;; Marginalia: Load after init
(use-package marginalia
  :straight t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; Wgrep: Load on demand
(use-package wgrep
  :straight t
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-auto-save-buffer t))

;; Vertico-posframe: Load with vertico
(use-package vertico-posframe
  :straight t
  :after vertico
  :commands (vertico-posframe-mode)
  :hook (vertico-mode . vertico-posframe-mode))

;; Vertico-multiform: Load with vertico
;; (use-package vertico-multiform
;;   :straight t
;;   :after vertico
;;   :commands (vertico-multiform-mode)
;;   :hook (vertico-mode . vertico-multiform-mode)
;;   :config
;;   (defun +vertico-highlight-directory (file)
;;     "Highlight directories in file candidates."
;;     (when (string-suffix-p "/" file)
;;       (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
;;     file)
;;   (add-to-list 'vertico-multiform-categories
;;                '(file
;;                  (+vertico-transform-functions . +vertico-highlight-directory))))

;; (use-package company
;;   :straight t)

;; (use-package corfu
;;   :straight t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   (corfu-preview-current nil)    ;; Disable current candidate preview
;;   (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   (corfu-on-exact-match nil)     ;; Configure handling of exact matches

;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (vterm-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))
;;   :init
;;   (global-corfu-mode)

;;   ;; Enable optional extension modes:
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode))

;; (use-package corfu
;;   :straight (:host github :repo "minad/corfu" :branch "main")
;;   :demand t
;;   :custom
;;   (corfu-auto t)                 ;; Enable auto-completion
;;   (corfu-cycle t)                ;; Cycle through candidates
;;   (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   (corfu-quit-at-boundary nil)   ;; Don't quit at completion boundary
;;   (corfu-quit-no-match t)        ;; Quit if no match
;;   (corfu-preview-current nil)    ;; Disable current candidate preview
;;   :init
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode 1)       ;; Show documentation popups
;;   :config
;;   (setq corfu-popupinfo-delay '(0.5 . 0.2)) ;; Adjust popup delay for responsiveness
;;   :bind (:map corfu-map
;;               ("C-n" . corfu-next)
;;               ("C-p" . corfu-previous)
;;               ("TAB" . corfu-insert)
;;               ("C-g" . corfu-quit)))

;; (use-package cape
;;   :straight t
;;   :demand t
;;   :init
;;   ;; Add useful CAPF backends
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Buffer completions
;;   (add-to-list 'completion-at-point-functions #'cape-file)    ;; File paths
;;   (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keywords
;;   ;; Org-roam completions
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (add-to-list 'completion-at-point-functions #'org-roam-complete-link-at-point)
;;               (add-to-list 'completion-at-point-functions #'org-roam-complete-everywhere)))
;;   :config
;;   ;; Enable org-roam completion everywhere
;;   (setq org-roam-completion-everywhere t)
;;   ;; :bind (("C-c p d" . cape-dabbrev)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ("C-c p k" . cape-keyword))
;;   )

(provide 'completion)
;;; completion.el ends here
