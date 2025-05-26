(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package system-packages
  :ensure t)

(defun frostyx/guix (&key install)
  (let ((system-packages-package-manager 'guix)
        (system-packages-use-sudo nil))
    (or (frostyx/rpm-query install)
 				(system-packages-install install))))

(defun frostyx/rpm-query (pack)
  (equal 0 (shell-command
            (concat "rpm -q " pack))))

(setq system-packages-package-manager 'guix)
(setq system-packages-use-sudo t)

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :init
	(setq display-line-numbers 'relative
        display-line-numbers-width 3))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(setq display-line-numbers 'relative)

(use-package no-littering
  :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 365
        auto-package-update-prompt-before-update nil
        auto-package-update-hide-results t)
  (auto-package-update-at-time "04:00"))

(use-package vterm
  :ensure t)

(use-package multi-vterm
	:bind (:map global-map ("C-x e" . multi-vterm))
	:ensure t)

(use-package bufler
  :bind (:map global-map ("C-x b" . bufler-switch-buffer))
  :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el"
                  :files (:defaults (:exclude "helm-bufler.el")))
  :config
  (use-package helm-bufler
    :quelpa (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
                         :files ("helm-bufler.el")))
  (setf bufler-groups
        (bufler-defgroups
					;; Группа для ~/Templates2/Rust/t12stat и vterm буферов в этой директории
					(group
           (group-or "t12stat"
                     (dir "~/Templates2/Rust/t12stat")
                     (group-and "vterm in t12stat"
																(mode-match "vterm" (rx bos "vterm-"))
																(lambda (buffer)
																	(with-current-buffer buffer
																		(when (and (bound-and-true-p vterm-default-directory)
                                               (string-prefix-p "~/Templates2/Rust/t12stat"
																																(expand-file-name vterm-default-directory)))
																			"t12stat"))))))
					;; Остальные группы из стандартной конфигурации
					(group
           (auto-workspace))
					(group
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
					(group
           (group-and "*Special*"
											(lambda (buffer)
												(unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
																		(funcall (mode-match "Dired" (rx bos "dired")) buffer)
																		(funcall (auto-file) buffer))
													"*Special*")))
           (group
						(name-match "**Special**"
												(rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
						(mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
						(auto-directory))
           (mode-match "*Helm*" (rx bos "helm-"))
           (auto-mode))
					(dir user-emacs-directory)
					(group
           (dir (if (bound-and-true-p org-directory)
										org-directory
									"~/org"))
           (group
						(auto-indirect)
						(auto-file))
           (group-not "*special*" (auto-file))
           (auto-mode))
					(group
           (auto-projectile))
					(group
           (auto-project))
					(auto-directory)
					(auto-mode))))

;; (use-package reader
;; 	:ensure t
;;    :straight '(reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
;;   										:files ("reader.el" "render-core.so")
;;   										:pre-build ("make" "all")))

