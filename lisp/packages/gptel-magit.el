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
  "You are an expert at writing short, clear Git commit messages. Your job is to summarize changes from the diff or context using simple words and minimal details, with Gitmoji codes for clarity.\n\nThe commit message MUST follow this structure:\n\n    <gitmoji> <type>(<optional scope>): <description>\n\n    [optional body]\n\nRules:\n- Use a type and Gitmoji code from this list:\n  - add: :heavy_plus_sign: Add new files or functionality\n  - build: :package: Update build system or dependencies\n  - chore: :wrench: Update tools or minor tasks\n  - ci: :construction_worker: Update CI system\n  - config: :gear: Update config files\n  - docs: :books: Update docs\n  - feat: :sparkles: Add new feature\n  - fix: :bug: Fix bug\n  - i18n: :globe_with_meridians: Update translations\n  - perf: :zap: Improve performance\n  - refactor: :recycle: Rework code, no new features or fixes\n  - remove: :heavy_minus_sign: Remove files or functionality\n  - security: :lock: Fix security issues\n  - style: :art: Update formatting\n  - test: :rotating_light: Add or update tests\n\n- Place Gitmoji code (e.g., :sparkles:) before type, with a space.\n- Add optional scope to show the affected part of code (e.g., fix(parser)).\n\n- **Hexagonal architecture scopes (optional):**\n  - If the diff clearly belongs to a known layer, use: \n\n        (<layer>/<module>)\n\n    Example: feat(adapters/db): Add repository\n\n  - If the layer is **not explicitly visible**, **do NOT guess**. Use only the module:\n\n        feat(gptel-magit): Update prompts\n\n  Allowed layers:\n  - app\n  - domain\n  - application\n  - adapters\n  - infra\n\n- Keep the description short and simple. Use basic words (e.g., \"Add login page\").\n- Capitalize description, no punctuation at the end.\n- Keep the subject line (Gitmoji + type + scope + description) ideally under 50 characters, max 72.\n- Write in English and avoid complex grammar.\n- Analyze the diff to choose type, Gitmoji, and scope.\n- If changes span multiple types, choose the most significant.\n\n- **Use the commit body only when absolutely necessary**, such as:\n  - very large diffs needing brief bullets\n  - BREAKING CHANGE notes\n  - ticket references\n\n  If the summary fits cleanly in the subject line, **do not add a body**.\n\n- For breaking changes, use :boom: with \"feat\" or \"fix\" and add \"BREAKING CHANGE:\" in the body.\n- If diff is unclear or empty, create a generic \"chore\" commit with a note to review manually.\n\nExamples of valid commit messages:\n- :sparkles: feat(auth): Add login page\n- :bug: fix(parser): Fix space parsing\n- :books: docs(readme): Update install guide\n- :recycle: refactor(api): Simplify error code\n- :heavy_plus_sign: add(ui): Add button component\n- :sparkles: feat(adapters/db): Add repository\n- :sparkles: feat(gptel-magit): Integrate gptel for commit messages\n\nExample with body (only when needed):\n- :boom: feat(api): Change response format\n\n  - Update response structure\n  - BREAKING CHANGE: Response now uses {data: {}}\n\nInvalid examples (avoid):\n- Update stuff\n- :bug: fix: fixed bug.\n- :rocket: FEAT: Add thing\n"
  "Prompt for short, simple commit messages with Gitmoji codes for AIMLAPI."
  :type 'string
  :group 'gptel-magit)

(defcustom gptel-magit-model "gpt-4o"
  "The model to use for AIMLAPI."
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
  "Отправляет запрос с экранированным diff."
  (if (string-empty-p (string-trim diff))
      (progn
        (message "Ошибка: Пустой diff, нет изменений для коммита")
        (funcall (plist-get args :callback) nil `(:error "Empty diff")))
    (let* ((encoded-diff (encode-coding-string diff 'utf-8 t))
           (escaped-diff (json-encode-string encoded-diff))
           (url "https://api.aimlapi.com/chat/completions")
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " (getenv "AIML_API")))))
           (request-data (gptel-magit--url-request-data escaped-diff args))
           (url-request-data
            (encode-coding-string
             (json-encode request-data)
             'utf-8 t))
           (callback (plist-get args :callback)))
      ;; (message "Экранированный diff: %s" escaped-diff)
      ;; (message "Отправляемый JSON: %s" (json-encode request-data)) ; Debug log
      (url-retrieve url
                    (lambda (status)
                      (let* ((raw-response (buffer-string))
                             (http-status (or (plist-get status :code) 0))
                             (response (condition-case err
                                           (with-temp-buffer
                                             (insert raw-response)
                                             (goto-char (point-min))
                                             (when (re-search-forward "\n\n" nil t)
                                               (json-read)))
                                         (error (message "Ошибка парсинга JSON: %s" err) nil)))
                             (parsed-response
                              (cond
                               ((null response)
                                (message "Ошибка: Пустой или невалидный JSON")
                                nil)
                               ((alist-get 'error response)
                                (message "API error: %s" (alist-get 'error response))
                                nil)
                               (t
                                (let ((choices (alist-get 'choices response)))
                                  (if (and choices (vectorp choices) (> (length choices) 0))
                                      (or (alist-get 'content
                                                     (alist-get 'message (aref choices 0)))
                                          (message "Ошибка: Нет content в message")
                                          nil)
                                    (message "Ошибка: Неверный формат choices: %s" choices)
                                    nil)))))
                             (info `(:http-status ,http-status
                                     :raw-response ,raw-response
                                     :error ,(if (plist-get status :error)
                                                 (format "%s" (plist-get status :error))
                                               (unless parsed-response "Failed to parse response")))))
                        ;; (message "Сырой ответ API: %s" raw-response)
                        ;; (message "Информация API: %s" info)
                        (funcall callback parsed-response info)))
                    nil t t))))

(defun gptel-magit-generate-message ()
  "Создаёт сообщение для коммита с помощью AIMLAPI."
  (interactive)
  (let ((diff (magit-git-output "diff" "--cached")))
    (message "Diffing changes to be committed")
    (gptel-magit--request
     diff
     :system gptel-magit-commit-prompt
     :callback
     (lambda (response info)
       (if response
           (with-current-buffer (current-buffer)
             (goto-char (point-min))
             (insert response)
             ;; (message "Commit message inserted: %s" response)
						 )
         (message "Failed to generate commit message: %s" (plist-get info :error)))))))

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
