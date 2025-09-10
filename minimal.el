(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(require 'package)
(setq package-archives
      '(("myelpa" . "~/.myelpa/")
	("melpa" . "http://1.15.88.122/melpa/packages/")
	("gnu"   . "http://1.15.88.122/gnu/")
	("nongnu" . "http://1.15.88.122/gnu/nongnu/")))

;; Straight.el bootstrap (unchanged, as it’s required early)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(setq straight-use-package-by-default t)
(require 'use-package)

;; Initialize package.el (unchanged, but moved before use-package bootstrap)
(package-initialize)

;; Bootstrap use-package (optimized to avoid redundant installation)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Don’t auto-install unless :straight t
(setq use-package-always-defer nil)   ;; Default to deferring all packages


;; Other settings (unchanged, but included for context)
(server-start)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(electric-pair-mode)
(global-auto-revert-mode t)
(define-coding-system-alias 'UTF-8 'utf-8)

;; Custom file and other settings (unchanged)
(setq custom-file (concat user-emacs-directory "custom.el")
      mouse-yank-at-point t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      initial-scratch-message ";; Do you even lisp, bro? ಠ_ಠ\n\n\n"
      backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
      kill-buffer-query-functions nil
      evil-shift-width 2
      tab-width 2
      mouse-autoselect-window t)

(setq-default shell-file-name "/bin/fish")

;; Configure Org mode
(use-package org
  :config
  (setq org-directory "~/Org/agenda/GTD/")
  (setq org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DONE(d)")))
  (setq org-log-done 'time))

(use-package annotate
  :straight t
  :hook (prog-mode . annotate-mode))

(use-package company
  :straight t)

(use-package eglot
  :straight (:type built-in)
  :defer t
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq eglot-inlay-hints-mode t)
  (push :documentHighlightProvider eglot-ignored-server-capabilities))

(use-package rustic
  :straight t
  ;; :mode ("\\.rs\\'" . rustic-mode)
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
  (setq rustic-lsp-client 'eglot)
  (setq eldoc-idle-delay 0.0))

(use-package format-all
  :straight t
  :commands (format-all-buffer format-all-mode)
  :defer t)

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
        magit-auto-revert-mode t)
  :config
  (custom-set-faces
   '(magit-section-heading ((t (:foreground "#61afef" :weight bold))))
   '(magit-branch-local ((t (:foreground "#98c379"))))
   '(magit-branch-remote ((t (:foreground "#e06c75"))))
   '(magit-diff-added ((t (:background "#3d4f3d" :foreground "#98c379"))))
   '(magit-diff-removed ((t (:background "#4f3d3d" :foreground "#e06c75"))))))

(use-package git-gutter
  :straight t
  :commands (git-gutter-mode)
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :straight t
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  :if (display-graphic-p))

