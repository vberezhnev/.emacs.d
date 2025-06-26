;;; completion.el -*- lexical-binding: t; -*-

;; Убедитесь, что у вас установлены необходимые пакеты:
;; vertico, orderless, consult, consult-dir, consult-flycheck, consult-yasnippet,
;; embark, marginalia, wgrep, vertico-posframe, vertico-multiform

;; Требуется для корректной работы некоторых функций
(require 'recentf)
(require 'minibuffer)

;;; Vertico
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  ;; Используем consult-completion-in-region, если vertico-mode активен
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  ;; Привязки клавиш для vertico-map
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  ;; Очистка путей при навигации по директориям
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; Скрытие буфера *Completions* для проблемных команд
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))

;;; Orderless
(use-package orderless
  :ensure t
  :hook (after-init . (lambda () (setq completion-styles '(orderless basic))))
  :config
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
  ;; Убираем наследование для completions-first-difference
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;; Consult
(use-package consult
  :ensure t
  :bind (;; Переопределение стандартных команд
         ([remap bookmark-jump] . consult-bookmark)
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
  ;; Включаем recentf-mode для consult-recent-file и consult-buffer
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
  ;; Настройка предпросмотра для некоторых команд
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any)))

;;; Consult-dir
(use-package consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; Добавляем источники для SSH и локальных директорий
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

;;; Consult-flycheck
(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

;;; Consult-yasnippet
(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind ([remap yas-insert-snippet] . consult-yasnippet))

;;; Embark
(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)
         ;; :prefix-map embark-prefix
         ;; (:prefix "C-h" . embark-prefix-help-command)
	 )
  ;; :config
  ;; (require 'consult)
  ;; Отключаем which-key для embark
  ;; (setq which-key-use-C-h-commands nil
  ;;       prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :ensure t)

;;; Marginalia
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;;; Wgrep
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;;; Vertico-posframe (опционально, если вы хотите использовать childframe)
(use-package vertico-posframe
  :ensure t
  :hook (vertico-mode . vertico-posframe-mode))

;;; Vertico-multiform
(use-package vertico-multiform
  ;; :ensure t
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defun +vertico-highlight-directory (file)
    "Highlight directories in file candidates."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)
  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory))))

;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   :bind
;;   (:map corfu-map
;;         ("C-n" . corfu-next)
;;         ("C-p" . corfu-previous)
;;         ("M-<" . corfu-first)
;;         ("M->" . corfu-last)
;;         ("TAB" . corfu-complete)
;;         ([tab] . corfu-complete))
;;   :init
;;   (global-corfu-mode))

;; (defun corfu-yasnippet-or-completion ()
;;   (interactive)
;;   (or (do-yas-expand)
;;       (corfu-complete)))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "::") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (corfu-complete)
;;           (indent-for-tab-command)))))

(provide 'completion)
;;; completion.el ends here
