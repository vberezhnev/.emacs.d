;;________________________________________________________________
;;;    Flycheck
;;________________________________________________________________

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.2)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 2)
	:config
	;; enable typescript-tslint checker
	(flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'flycheck-setting)
