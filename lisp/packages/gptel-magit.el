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
  "You are an expert at writing short, clear Git commit messages. Your job is to summarize changes from the diff or context using simple words and minimal details.\n\nThe commit message MUST follow this structure:\n\n    <type>(<scope>): <description>\n\n    [optional body]\n\nRules:\n- Use a type from this list:\n  - add: Add new files or functionality\n  - build: Update build system or dependencies\n  - chore: Update tools or minor tasks\n  - ci: Update CI system\n  - config: Update config files\n  - docs: Update docs\n  - feat: Add new feature\n  - fix: Fix bug\n  - i18n: Update translations\n  - perf: Improve performance\n  - refactor: Rework code, no new features or fixes\n  - remove: Remove files or functionality\n  - security: Fix security issues\n  - style: Update formatting\n  - test: Add or update tests\n- Add scope for codebase section (e.g., sessions-manager-rs, parser).\n- Keep description short, simple, in lowercase, using basic words (e.g., \"add login page\").\n- No punctuation at end of description.\n- Keep subject line (type, scope, description) under 50 characters if possible, max 72.\n- Write in English, avoid complex grammar or tenses.\n- Analyze diff to pick type and scope (e.g., test files -> \"test\", module \"auth\" -> scope \"auth\").\n- Choose most significant type if changes cover multiple types.\n- Add body only for complex changes or breaking changes, using short bullet points.\n- For breaking changes, add \"BREAKING CHANGE:\" in body.\n- Include ticket/issue numbers in body if available (e.g., \"Closes #123\").\n- Do not use Gitmoji, emojis, or uppercase in description.\n\nExamples of valid commit messages:\n- docs(sessions-manager-rs): improve docs & add changelog\n- feat(auth): add login page\n- fix(parser): handle space parsing\n- refactor(api): simplify error code\n\nExample with body:\n- feat(api): change response format\n\n  - update response structure\n  - update docs\n  - BREAKING CHANGE: response now uses {data: {}}\n  - Closes #123\n\nInvalid examples (avoid these):\n- Update stuff (too vague)\n- fix: fixed bug. (punctuation, not simple)\n- FEAT: Add thing (uppercase, vague)"
  "Prompt for short, simple commit messages without Gitmoji for AIMLAPI."
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
  ;; (transient-append-suffix 'magit-diff "s"
  ;;   '("x" "Explain" gptel-magit-diff-explain))
	)

(provide 'gptel-magit)
;;; gptel-magit.el ends here
