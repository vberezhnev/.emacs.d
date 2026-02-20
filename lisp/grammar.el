;;; grammar.el --- Grammar and Spelling configuration (EN/RU/ZH) -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; 1. Базовая орфография (Flyspell + Aspell)
;; -----------------------------------------------------------------------------
(use-package flyspell
  :straight t
  :defer t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (org-mode . flyspell-mode))
  :config
  (require 'flyspell)

  ;; --- CRITICAL NIX FIXES ---
  (setq ispell-program-name "aspell")
  
  ;; Явно задаем параметры словарей, чтобы избежать ошибки "Encoding nil"
  ;; Это объясняет Emacs'у, что словари используют UTF-8
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          ("ru" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "ru_ru") nil utf-8)))

  (setq ispell-dictionary "en_US")
  (setq ispell-silently-savep t)
  (setq flyspell-issue-message-flag nil)
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")

  ;; Навигация
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-auto-correct-previous-word)
  
  ;; --- PREDICATE: Ignore Code & Chinese ---
  (defun my/org-flyspell-predicate ()
    "Игнорировать код, ссылки и китайские иероглифы."
    (let ((face (get-char-property (point) 'face))
          (word (thing-at-point 'word t)))
      (and 
       (not (or (memq face '(org-code org-verbatim org-link org-meta-line))
                (save-excursion (org-in-src-block-p))))
       (not (and word (string-match-p "\\cc" word))))))

  (defun my/flyspell-org-setup ()
    (setq-local flyspell-generic-check-word-predicate 'my/org-flyspell-predicate))
  
  (add-hook 'org-mode-hook 'my/flyspell-org-setup))

;; -----------------------------------------------------------------------------
;; 2. Flyspell Lazy
;; -----------------------------------------------------------------------------
(use-package flyspell-lazy
  :straight t
  :after flyspell
  :hook (flyspell-mode . flyspell-lazy-mode)
  :config
  ;; Fix for native-comp warning
  (unless (fboundp 'flyspell-mode-off)
    (defalias 'flyspell-mode-off 'flyspell-mode))
  
  (setq flyspell-lazy-idle-seconds 1)
  (setq flyspell-lazy-window-idle-seconds 3))

;; -----------------------------------------------------------------------------
;; 3. Интерфейс исправлений
;; -----------------------------------------------------------------------------
(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-wrapper)))

;; -----------------------------------------------------------------------------
;; 4. Грамматика LanguageTool (Nix Compatible)
;; -----------------------------------------------------------------------------
(use-package langtool
  :straight t
  :commands (langtool-check langtool-switch-default-language)
  :config
  ;; NIX FIX: Не ищем JAR файлы. Используем бинарную обертку из PATH.
  ;; Пакет `languagetool` в Nix создает команду `languagetool-commandline`
  (setq langtool-bin "languagetool-commandline")
  
  ;; Аргументы для Java (важно для UTF-8)
  (setq langtool-java-arguments '("-Dfile.encoding=UTF-8"))

  (setq langtool-default-language "en-US")
  ;; Отключаем правила, которые мешают в Org-mode
  (setq langtool-disabled-rules '("WHITESPACE_RULE" "EN_QUOTES"))
  
  :bind (:map org-mode-map
              ("C-c L c" . langtool-check)
              ("C-c L d" . langtool-check-done)
              ("C-c L b" . langtool-switch-default-language)))

;; -----------------------------------------------------------------------------
;; 5. Writegood-mode
;; -----------------------------------------------------------------------------
(use-package writegood-mode
  :straight t
  :hook (org-mode . writegood-mode)
  :bind (:map org-mode-map
              ("C-c g m" . writegood-mode)))

;; -----------------------------------------------------------------------------
;; 6. Умное переключение (Robust Version)
;; -----------------------------------------------------------------------------
(defvar my/current-lang-engine "en" "Current language state.")

(defun my/switch-language-engine ()
  "Cycle: EN -> RU -> ZH with error handling."
  (interactive)
  (condition-case err
      (cond
       ;; -> RUSSIAN
       ((string= my/current-lang-engine "en")
        (ispell-change-dictionary "ru") 
        (setq langtool-default-language "ru-RU")
        (setq my/current-lang-engine "ru")
        (message "Language: RUSSIAN (Spell: ru, Grammar: ru-RU)"))

       ;; -> CHINESE
       ((string= my/current-lang-engine "ru")
        ;; Для ZH используем EN словарь (игнор иероглифов)
        (ispell-change-dictionary "en_US") 
        (setq langtool-default-language "zh-CN")
        (setq my/current-lang-engine "zh")
        (message "Language: CHINESE (Spell: en_US, Grammar: zh-CN)"))

       ;; -> ENGLISH
       ((string= my/current-lang-engine "zh")
        (ispell-change-dictionary "en_US")
        (setq langtool-default-language "en-US")
        (setq my/current-lang-engine "en")
        (message "Language: ENGLISH (Spell: en_US, Grammar: en-US)")))
    
    ;; Error Handler
    (error
     (message "Error switching: %s. Did you run 'home-manager switch'?" (error-message-string err))
     ;; Fallback to safe English
     (setq my/current-lang-engine "en")
     (ispell-change-dictionary "en_US")))

  ;; Refresh buffer
  (when (bound-and-true-p flyspell-mode)
    (flyspell-buffer)))

(global-set-key (kbd "C-c d") 'my/switch-language-engine)

(provide 'grammar)
