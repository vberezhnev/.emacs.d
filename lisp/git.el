;;; git.el --- Git and version control configuration -*- lexical-binding: t; -*-

(require 'org)

;; Auth sources (unchanged, lightweight global setting)
(setq auth-sources '("~/.authinfo"))

;; Magit: Load for git commands
(use-package magit
  :straight t
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk 'all
        magit-section-initial-visibility-alist '((untracked . show)
                                                 (unstaged . show)
                                                 (staged . show))
        magit-status-margin '(t age magit-log-margin-width nil 18)
        magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-commit-show-diff t
        magit-revision-show-gravatars nil
        magit-refresh-status-buffer t
        magit-diff-paint-whitespace t
        magit-auto-revert-mode t
	global-auto-revert-mode 1)
  :config
  (custom-set-faces
   '(magit-section-heading ((t (:foreground "#61afef" :weight bold))))
   '(magit-branch-local ((t (:foreground "#98c379"))))
   '(magit-branch-remote ((t (:foreground "#e06c75"))))
   '(magit-diff-added ((t (:background "#3d4f3d" :foreground "#98c379"))))
   '(magit-diff-removed ((t (:background "#4f3d3d" :foreground "#e06c75"))))))

;; Conventional-commit: Load for git commit mode
;; (use-package conventional-commit
;;   :straight (:type git :host github :repo "akirak/conventional-commit.el" :branch "master")
;;   :commands (conventional-commit-setup)
;;   :hook (git-commit-mode . conventional-commit-setup))

;; Magit-todos: Load after magit
(use-package magit-todos
  :straight t
  :after magit
  :commands (magit-todos-mode)
  :config
  (magit-todos-mode 1))

;; Git-gutter: Load for programming modes
(use-package git-gutter
  :straight t
  :commands (git-gutter-mode)
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:update-interval 0.02))

;; Git-gutter-fringe: Load for graphical displays
(use-package git-gutter-fringe
  :straight t
  :after git-gutter
  ;; :commands (git-gutter-fringe-mode)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  :if (display-graphic-p))

;; Transient: Load on demand
(use-package transient
  :straight t
  :commands (transient-define-prefix transient-dispatch)
  :defer t)

(use-package forge
  :straight t)

(provide 'git)
;;; git.el ends here
