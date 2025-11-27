(use-package flyspell
  :straight t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))  ; Только комментарии в коде
  :config
  (setq ispell-program-name "aspell")  ; Или "hunspell"
  (setq ispell-dictionary "en_US")     ; По умолчанию английский
  ;; Переключение словаря: C-c $ или M-x ispell-change-dictionary
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")  ; Быстрые подсказки
)

;; Русский словарь (добавь в ispell-local-dictionary-alist если нужно)
;; (ispell-change-dictionary "ru" nil)  ; Для русского буфера

;; 2. Грамматика с LanguageTool
;; (use-package langtool
;;   :straight t
;;   :defer t
;;   :commands (languagetool-check
;;              languagetool-clear-suggestions
;;              languagetool-correct-at-point
;;              languagetool-correct-buffer
;;              languagetool-set-language
;;              languagetool-server-mode
;;              languagetool-server-start
;;              languagetool-server-stop)
;;   :config
;;   (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;         languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
;;         languagetool-server-command "~/.languagetool/languagetool-server.jar")
;;   (setq langtool-default-language "en-US")  ; Или "ru-RU"
;;   (setq langtool-server-http-port 8081)
;;   ;; Игнорировать правила: (setq langtool-disabled-rules '("WHITESPACE_RULE"))
;;   :bind (("C-c l c" . langtool-check)      ; Проверить буфер
;;          ("C-c l d" . langtool-check-done) ; Закончить
;;          ("C-c l b" . langtool-switch-default-language)))

;; - Аббревиатуры (abbrev) — свои сокращения
;; (use-package abbrev
;;   :diminish abbrev-mode
;;   :hook (text-mode . abbrev-mode)
;;   :config
;;   (define-abbrev-table 'global-abbrev-table
;;     '(("teh" "the") ("rus" "русский") ("eng" "english")))  ; Добавь свои
;; )

;; - Синонимы (M-x powerthesaurus-install если нужно)
;; (use-package powerthesaurus
;;   :ensure t
;;   :bind ("C-c s" . powerthesaurus-lookup-dwim-current-word)
;; )

;; - Переключение языков
;; (defun my/switch-dictionary ()
;;   (interactive)
;;   (let* ((dict (if (string= ispell-current-dictionary "en_US") "ru" "en_US")))
;;     (ispell-change-dictionary dict)
;;     (message "Dictionary switched to: %s" dict)))
;; ;; (global-set-key (kbd "C-c d") 'my/switch-dictionary)
