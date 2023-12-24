(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
	magit-todos-depth 4
	magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*" "node_modules/" "target/"))
  (custom-set-variable
   '(magit-todos-keywords (list "TODO" "FIXME" "BUGFIX" "HACK"))))

(use-package blamer
  :defer nil
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (setq blamer-view 'overlay
        blamer-type 'posframe-popup
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
        blamer-author-formatter " ✎ [%s] - "
        blamer-commit-formatter "● %s ● ")

  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (forge-browse-commit commit-hash))))

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))
  (global-blamer-mode 1))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :defer nil
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :defer nil
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(provide 'git-setting)
