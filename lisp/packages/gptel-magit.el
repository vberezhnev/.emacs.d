;;; gptel-magit.el --- Generate commit messages for magit using AIMLAPI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Authors
;; SPDX-License-Identifier: Apache-2.0

;; Author: Ragnar Dahlén <r.dahlen@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (magit "4.0"))
;; Keywords: vc, convenience
;; URL: https://github.com/ragnard/gptel-magit

;;; Commentary:

;; This package integrates with api.aimlapi.com to generate commit messages for magit.

;;; Code:

(require 'magit)
(require 'url)
(require 'json)

(defcustom gptel-magit-commit-prompt
  "You are an expert at writing Git commit messages. Summarize changes from the diff using Gitmoji.

The commit message MUST follow this structure:
<gitmoji> <type>(<scope>): <description>

### SCOPE RULES (STRICT):
1. Look at the file paths in the diff (e.g., `modified  path/to/file.ts`).
2. **Path Cleaning**: Ignore project root prefixes (like `grip-class-backend/` or `grip-class-frontend/`). Focus on what comes after `src/`.
3. **Hexagonal Check**: 
   - If the path contains `frameworks/primary`, `frameworks/adapters`, `infrastructure`, or `domain`, you MUST use the format: (<layer>/<module>).
   - Example: `src/frameworks/primary/guards/jwt.ts` -> (primary/guards)
   - Example: `src/infrastructure/db/repo.ts` -> (infra/db)
4. **Standard Check**: 
   - If it is not hexagonal (e.g. frontend components), use: (<folder>/<module>).
   - Example: `src/components/api/auth.tsx` -> (components/auth)
   - Example: `app/pages/login.tsx` -> (pages/login)
5. **Conflict**: If multiple files are changed, use the scope of the most 'logic-heavy' file (usually the backend one or the main component). NEVER use a single word if a layer is visible.

### GENERAL RULES:
- Use Gitmoji: feat :sparkles:, fix :bug:, refactor :recycle:, chore :wrench:, docs :books:, style :art:, test :rotating_light:.
- Description: Simple English, Capitalized, no period at the end.
- Max 72 characters for the first line.

### EXAMPLES:
- :bug: fix(primary/guards): Skip dev key if auth header present
- :sparkles: feat(components/auth): Clear dev key on logout
- :wrench: chore(infra/config): Update env variables"
  "Prompt for short, simple commit messages with Gitmoji codes for AIMLAPI."
  :type 'string
  :group 'gptel-magit)

(defcustom gptel-magit-model "qwen3-coder:30b" ; Указываем твою модель
  "The model to use for Ollama."
  :type 'string
  :group 'gptel-magit)

(defun gptel-magit--format-commit-message (message)
  "Форматирует сообщение коммита MESSAGE, обрабатывая nil."
  (if (null message)
      (progn
        (message "Ошибка: API вернул пустое сообщение")
        "")
    (with-temp-buffer
      (insert message)
      (text-mode)
      (setq fill-column git-commit-summary-max-length)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun gptel-magit--url-request-data (prompt &optional parameters)
  "Создаёт JSON для запроса к AIMLAPI."
  (let* ((params (or parameters (list :system gptel-magit-commit-prompt)))
         (data `((model . ,gptel-magit-model)
                 (messages . [((role . "system") (content . ,(plist-get params :system)))
                              ((role . "user") (content . ,prompt))])
                 (max_tokens . 500)
                 (stream . :json-false)))) ; Force boolean
    ;; (message "Запрос JSON: %s" (json-encode data))
    data))

(defun gptel-magit--request (diff &rest args)
  "Отправляет запрос к Ollama через внешний процесс curl для максимальной скорости."
  (if (string-empty-p (string-trim diff))
      (message "Ошибка: Пустой diff")
    (let* ((url "http://localhost:11434/v1/chat/completions")
           (callback (plist-get args :callback))
           (request-data `((model . ,gptel-magit-model)
                           (messages . [((role . "system") (content . ,gptel-magit-commit-prompt))
                                        ((role . "user") (content . ,diff))])
                           (stream . :json-false)
                           (temperature . 0.2))) ; Низкая температура для скорости и четкости
           (json-payload (encode-coding-string (json-encode request-data) 'utf-8))
           (output-buffer (generate-new-buffer " *gptel-ollama-output*")))
      
      (message "Ollama: Generating with %s..." gptel-magit-model)
      
      (make-process
       :name "gptel-magit-curl"
       :buffer output-buffer
       :command `("curl" "-s" "-X" "POST" ,url
                  "-H" "Content-Type: application/json"
                  "-d" ,json-payload)
       :sentinel
       (lambda (process event)
         (when (eq (process-status process) 'exit)
           (let ((parsed-content nil)
                 (http-status (process-exit-status process)))
             (unwind-protect
                 (with-current-buffer (buffer-name output-buffer)
                   (goto-char (point-min))
                   (condition-case err
                       (let* ((data (json-read))
                              (choices (alist-get 'choices data)))
                         (if (and (vectorp choices) (> (length choices) 0))
                             (setq parsed-content 
                                   (alist-get 'content (alist-get 'message (aref choices 0))))
                           (message "Ollama Error: %s" (buffer-string))))
                     (error (message "Parse Error: %s" err))))
               (funcall callback parsed-content `(:http-status ,http-status))
               (kill-buffer output-buffer)))))))))

(defun gptel-magit--get-optimized-diff ()
  "Получает сжатый diff: только измененные строки без лишнего контекста."
  ;; Используем -U0 чтобы убрать строки контекста вокруг изменений
  (let ((diff (magit-git-output "diff" "--cached" "-U0" "--diff-algorithm=histogram")))
    (if (> (length diff) 8000)
        (concat (substring diff 0 8000) "\n... [diff truncated for speed] ...")
      diff)))

(defun gptel-magit-generate-message ()
  "Создаёт сообщение для коммита, используя оптимизированный diff."
  (interactive)
  (let ((diff (gptel-magit--get-optimized-diff)))
    (if (string-empty-p diff)
        (message "No staged changes to commit.")
      (message "Ollama: Processing optimized diff...")
      (gptel-magit--request
       diff
       :system gptel-magit-commit-prompt
       :callback
       (lambda (response info)
         (if response
             (save-excursion
               (goto-char (point-min))
               (insert response))
           (message "Error: %s" (plist-get info :error))))))))

(defun gptel-magit--generate (callback)
  "Generate a commit message and pass it to CALLBACK."
  (let ((diff (magit-git-output "diff" "--cached")))
    (message "Diffing changes to be committed")
    (gptel-magit--request
     diff
     :system gptel-magit-commit-prompt
     :callback
     (lambda (response info)
       (if response
           (funcall callback (gptel-magit--format-commit-message response))
         (message "Failed to generate commit message: %s" (plist-get info :error)))))))

(defun gptel-magit-commit-generate (&optional args)
  "Create a new commit with a generated commit message."
  (interactive (list (magit-commit-arguments)))
  (gptel-magit--generate
   (lambda (message)
     (magit-commit-create (append args `("--message" ,message "--edit")))))
  (message "magit-gptel: Generating commit..."))

(defun gptel-magit--show-diff-explain (text)
  "Popup a buffer with diff explanation TEXT."
  (let ((buffer-name "*gptel-magit diff-explain*"))
    (when-let ((existing-buffer (get-buffer buffer-name)))
      (kill-buffer existing-buffer))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert text)
        (setq fill-column 72)
        (fill-region (point-min) (point-max))
        (markdown-view-mode)
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defun gptel-magit--do-diff-request (diff)
  "Send request for an explanation of DIFF."
  (gptel-magit--request diff
			:system "You are an expert at understanding and explaining code changes by reading diff output. Your job is to write a short clear summary explanation of the changes in Markdown format."
			:callback (lambda (response info)
				    (if (null response)
					(message "Ошибка: API вернул пустой ответ или ошибку: %s" info)
				      (gptel-magit--show-diff-explain response))))
  (message "magit-gptel: Explaining diff..."))

(defun gptel-magit-diff-explain ()
  "Ask for an explanation of diff at current section."
  (interactive)
  (when-let* ((section (magit-current-section))
              (start (oref section content))
              (end (oref section end))
              (content (buffer-substring start end)))
    (gptel-magit--do-diff-request content)))

;;;###autoload
(defun gptel-magit-install ()
  "Install gptel-magit functionality."
  (define-key git-commit-mode-map (kbd "M-g") 'gptel-magit-generate-message)
  (transient-append-suffix 'magit-commit "c"
    '("g" "Generate commit" gptel-magit-commit-generate))
  (transient-append-suffix 'magit-diff "s"
    '("x" "Explain" gptel-magit-diff-explain)))

(provide 'gptel-magit)
;;; gptel-magit.el ends here
