;;; gptel-magit.el --- Robust Git Commit Generator with Ollama -*- lexical-binding: t; -*-

(require 'gptel)
(require 'magit)

(defgroup gptel-magit nil
  "Генерация сообщений коммита через gptel и Ollama."
  :group 'magit)

(defcustom gptel-magit-model 'qwen2.5-coder:7b
  "Модель для генерации (например, qwen2.5-coder:7b или qwen3:4b)."
  :type 'symbol
  :group 'gptel-magit)

(defcustom gptel-magit-backend-name "Ollama"
  "Имя бэкенда Ollama, как оно указано в вашем gptel-make-ollama."
  :type 'string
  :group 'gptel-magit)

(defvar gptel-magit-commit-prompt
  "You are an expert at writing Git commit messages. 
Summarize changes from the diff using Gitmoji.
The commit message MUST follow this structure:
<gitmoji> <type>(<scope>): <description>

Output ONLY the raw text of the commit message. No markdown, no commentary."
  "Системный промпт для модели.")

;;;###autoload
(defun gptel-magit-generate-message (&optional rationale)
  "Генерирует сообщение коммита. Очищает буфер перед вставкой, чтобы не было каши."
  (interactive)
  (let* ((target-backend (gptel-get-backend gptel-magit-backend-name))
         (diff (shell-command-to-string "git diff --cached --unified=0 --no-prefix"))
         (commit-buf (or (get-buffer "COMMIT_EDITMSG")
                         (when (derived-mode-p 'git-commit-mode) (current-buffer))))
         (full-prompt (if (and rationale (not (string-empty-p rationale)))
                          (format "User Rationale: %s\n\nDiff:\n%s" rationale diff)
                        diff)))

    (unless target-backend (user-error "Бэкенд '%s' не найден!" gptel-magit-backend-name))
    (unless commit-buf (user-error "Буфер коммита не найден"))

    (if (string-empty-p (string-trim diff))
        (user-error "Diff пуст!")
      
      (message "[gptel-magit] Ollama генерирует чистый текст...")
      
      ;; Очищаем буфер перед началом, чтобы не было наслоения текста
      (with-current-buffer commit-buf
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))))

      (let ((gptel-backend target-backend)
            (gptel-model gptel-magit-model)
            (gptel-max-tokens 200))
        
        (gptel-request
         full-prompt
         :system gptel-magit-commit-prompt
         :stream t
         :callback
         (lambda (response info)
           (let ((msg-buffer commit-buf))
             (when (and (stringp response) (buffer-live-p msg-buffer))
               (with-current-buffer msg-buffer
                 (let ((inhibit-read-only t))
                   ;; Вставляем в конец текущего текста (стриминг)
                   (save-excursion
                     (goto-char (point-max))
                     (insert response))))))))))))

;;;###autoload
(defun gptel-magit-commit-with-rationale ()
  "Сначала спрашивает 'Зачем?', затем генерирует коммит."
  (interactive)
  (let ((rationale (read-string "Rationale (optional): ")))
    (gptel-magit-generate-message rationale)))

;;;###autoload
(defun gptel-magit-install ()
  "Заглушка для совместимости с вашим конфигом."
  (interactive)
  (message "gptel-magit helper ready."))

(provide 'gptel-magit)

;;; gptel-magit.el ends here
