;;________________________________________________________________
;;    Modeline
;;________________________________________________________________

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-lsp t
        doom-modeline-env-version t
        doom-modeline-indent-info t
        doom-modeline-buffer-encoding nil
        doom-modeline-enable-word-count t
        doom-modeline-vcs-max-length 20
        doom-modeline-major-mode-color-icon t
        doom-modeline-time-icon t
        doom-modeline-battery t
        doom-modeline-time t
        doom-modeline-workspace-name t
        doom-modeline-env-version t
        doom-modeline-modal-modern-icon t
        doom-modeline-modal-icon t
        doom-modeline-modal t)
  :config
  ;; (set-face-attribute 'region nil
  ;;                     :foreground (face-background 'doom-modeline-bar)
  ;;                     :background (face-background 'default))
  ;; (set-face-attribute 'highlight nil
  ;;                     :foreground (face-background 'doom-modeline-bar)
  ;;                     :background (face-background 'default))
  (eval-when-compile
    'company
    (doom-modeline-def-segment company-backend
      "Display the current company backend. `company-backend'."
      (when (company--active-p)
        (format "%s"
                (--map (s-replace "company-" "" (format "%s" it))
                       (if (listp company-backend) company-backend (list company-backend)))))))
  (doom-modeline-def-segment
    buffer-info
    "Overwrite of buffer info to not include the icon"
    (concat
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))
  (doom-modeline-def-segment
    buffer-type
    "Buffer icon and version if it exists"
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (when (and doom-modeline-env-version doom-modeline-env--version)
       (propertize
        (format "%s " doom-modeline-env--version)
        'face '(:height 0.7))))))

(use-package minions
  :delight " 𝛁"
  :hook (doom-modeline-mode . minions-mode)
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]"))

;;;; Modeline
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;;________________________________________________________________
;;    Treemacs
;;________________________________________________________________
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :defer nil
  :demand t
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :demand t
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :demand t
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :demand t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :init
  (treemacs-load-theme "doom-colors"))

(use-package lsp-treemacs
  :after treemacs
  :demand t
  :config
  (lsp-treemacs-sync-mode 1))

;;________________________________________________________________
;;    Projectile
;;________________________________________________________________

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-globally-ignored-files "node_modules"))

(defun my/highlight-todo-like-words ()
  (font-lock-add-keywords
   nil `(("\\<\\(FIXME\\|TODO\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'my/highlight-todo-like-words)

;;________________________________________________________________
;;    Helm
;;________________________________________________________________

(use-package helm
  :init
  ;; Настройка сочетаний клавиш для Helm
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  :demand t
  :config
  ;; Оптимизация Helm для улучшения производительности
  (setq
   ;; helm-candidate-number-limit 500
   ;;      helm-idle-delay 0.0
   ;;      helm-input-idle-delay 0.01
   helm-quick-update t
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   ;; helm-recentf-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-completion-in-region-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-move-to-line-cycle-in-source t
   helm-scroll-amount 8
   helm-ff-file-name-history-use-recentf nil
   helm-echo-input-in-header-line nil))

;;________________________________________________________________
;;    Dashboard
;;________________________________________________________________
(use-package dashboard
  ;; :straight (:build t)
  :ensure t
  :defer nil
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Welcome back, Darling!"
        dashboard-startup-banner "~/.emacs.d/images/Emacs-logo.svg"
        dashboard-center-content    t
        dashboard-show-shortcuts    nil
        dashboard-set-navigator     nil
        dashboard-set-heading-icons nil
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-set-file-icons    t)
  (setq dashboard-items '((recents  . 8)
                          (agenda   . 5)
                          (projects . 6)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

(provide 'ui-setting)
