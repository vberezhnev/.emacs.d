(setq auth-sources '("~/.authinfo"))

(use-package magit
	:ensure t
	:config
	(evil-leader/set-key
		"gg" 'magit
		"ga" 'magit-stage-file
		"gc" 'magit-commit
		"gp" 'magit-push-current
		"gc" 'magit-clone)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-section-initial-visibility-alist
        '((untracked . show)
          (unstaged . show)
          (staged . show)))
  (setq magit-status-margin '(t age magit-log-margin-width nil 18))
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-commit-show-diff t)
  (setq magit-revision-show-gravatars nil)
  (setq magit-refresh-status-buffer t)
  (setq magit-diff-paint-whitespace t)
  (setq magit-auto-revert-mode t)
  (custom-set-faces
   '(magit-section-heading ((t (:foreground "#61afef" :weight bold))))
   '(magit-branch-local ((t (:foreground "#98c379"))))
   '(magit-branch-remote ((t (:foreground "#e06c75"))))
   '(magit-diff-added ((t (:background "#3d4f3d" :foreground "#98c379"))))
   '(magit-diff-removed ((t (:background "#4f3d3d" :foreground "#e06c75"))))))

(use-package conventional-commit
  :straight (conventional-commit
             :type git
             :host github
             :repo "akirak/conventional-commit.el"
             :branch "master")
  :hook
  (git-commit-mode . conventional-commit-setup))

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

(use-package gptel
  :quelpa (gptel
					 :fetcher github
					 :repo "karthink/gptel"
					 :branch "master")
  :init
  (setq gptel-api-key (getenv "AIML_API"))
  (setq gptel-max-tokens 8024)
	(setq gptel-verbose t)
  :config
  (setq gptel-model 'gpt-4o
        gptel-backend
        (gptel-make-openai "AIMLAPI"
          :host "api.aimlapi.com"
          :endpoint "/chat/completions"
          :stream t
          :key gptel-api-key
          :models '(gpt-4o
                    gpt-4o-2024-08-06
                    gpt-4-turbo
                    chatgpt-4o-latest))
				)
  :bind (("M-s M-d" . gptel-context-add)
         ("M-s M-f" . gptel-add-file)
         ("M-s M-a" . gptel-menu)
         ("M-s M-r" . gptel--regenerate)
         ("M-s M-e" . gptel-rewrite)
         ("M-s M-s" . gptel)))

(use-package gptel-magit
  :load-path "~/.emacs.d/lisp/packages/"
  ;; :after (gptel magit)
	:init
  (setq gptel-api-key (getenv "AIML_API"))
  (setq gptel-max-tokens 8024)
	(setq gptel-verbose t)
  :config
  (setq gptel-magit-model 'gpt-4o)
  (setq gptel-magit-backend
        (gptel-make-openai "AIMLAPI"
          :host "api.aimlapi.com"
          :endpoint "/chat/completions"
          :stream nil
          :key (getenv "AIML_API")
          :models '(gpt-4o)))
	  (gptel-magit-install)

  :bind (:map git-commit-mode-map
              ("M-g" . gptel-magit-generate-message))
  :hook
  (magit-mode . gptel-magit-install))

(provide 'git)
;;; git.el ends here
