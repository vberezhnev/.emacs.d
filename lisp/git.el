;; Needed for Forge
(setq auth-sources '("~/.authinfo"))

(use-package magit
	:ensure t
	:config
	(evil-leader/set-key
		"gg" 'magit
		"ga" 'magit-stage-file
		"gc" 'magit-commit  ;; Maybe magit-commit-create
		"gp" 'magit-push-current)

	;; Общие настройки Magit
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; Открывать буферы в том же окне
  (setq magit-diff-refine-hunk 'all)     ; Подсвечивать изменения на уровне слов
  (setq magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (staged . show)))                  ; Разворачивать секции
  (setq magit-status-margin '(t age magit-log-margin-width nil 18)) ; Временные метки в статусе
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)) ; Формат лога
  (setq magit-commit-show-diff t)         ; Показывать diff при создании коммита
  (setq magit-revision-show-gravatars nil) ; Отключить gravatar

	;; Улучшение производительности
  (setq magit-refresh-status-buffer t)    ; Обновлять статус автоматически
  (setq magit-diff-paint-whitespace t)    ; Подсвечивать лишние пробелы
  (setq magit-auto-revert-mode t)         ; Автоматически обновлять буферы при изменениях в Git

	;; Визуальные улучшения
  (custom-set-faces
   '(magit-section-heading ((t (:foreground "#61afef" :weight bold)))) ; Цвет заголовков секций
   '(magit-branch-local ((t (:foreground "#98c379"))))               ; Цвет локальных веток
   '(magit-branch-remote ((t (:foreground "#e06c75"))))              ; Цвет удалённых веток
   '(magit-diff-added ((t (:background "#3d4f3d" :foreground "#98c379"))))   ; Цвет добавленных строк
   '(magit-diff-removed ((t (:background "#4f3d3d" :foreground "#e06c75")))))) ; Цвет удалённых строк

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))

(use-package git-gutter
	:ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
	:ensure t
	:config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; (use-package forge
;;   :ensure t
;;   :after magit)
