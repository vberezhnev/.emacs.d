;;; ai.el --- ai modules for magit, coding etc.

;;; Commentary:
;;

;;; Code:

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(use-package gptel-watch
  :straight (:host github :repo "ISouthRain/gptel-watch" :branch "master")
  :after gptel
  :config
  (gptel-watch-global-mode 1))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick" :branch "master")
  :bind (("C-c TAB" . gptel-quick))
  :custom
  (gptel-quick-word-count 20)
  (gptel-quick-use-context t)
  (gptel-quick-model 'gpt-4o)
  :config
  (setq gptel-quick-backend (gptel-get-backend "ChatGPT")))

(use-package gptel-autocomplete
  :straight (:host github :repo "JDNdeveloper/gptel-autocomplete" :branch "main")
  ;; :demand t
  :init
  (setq gptel-autocomplete-before-context-lines 100
        gptel-autocomplete-after-context-lines 20
        gptel-autocomplete-temperature 0.1
        gptel-autocomplete-use-context t)
  :bind (("C-c TAB" . gptel-complete)))

(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :demand t)

(use-package gptel
  ;; :straight (:host github :repo "karthink/gptel" :branch "master")
  :straight t
  :demand t
  :init
  (setq gptel-verbose t
        gptel-max-tokens 8024
        gptel-temperature 0.7
        gptel-api-key (getenv "AIML_API")
        gptel-include-reasoning nil
        gptel-track-media t
        gptel-confirm-tool-calls t
        gptel-model 'openai/gpt-5-2)
  :config
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all))

  (gptel-make-openai "OpenSearch"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key "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"
    :models '(deepseek/deepseek-r1-0528:free
              tngtech/deepseek-r1t2-chimera:free))

  (gptel-make-openai "ChatGPT"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(openai/gpt-5-2 gpt-4o openai/gpt-5-1-chat-latest openai/gpt-5-chat-latest openai/gpt-5-1-codex-mini))

  (gptel-make-openai "xAI"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(grok-code-fast-1 grok-4-1-fast-reasoning))

  (gptel-make-openai "Gemini"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(google/gemini-3-pro-preview))

  (gptel-make-openai "Nvidia"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(nvidia/nemotron-nano-9b-v2))

  (gptel-make-openai "Anthropic"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(anthropic/claude-opus-4-5 anthropic/claude-haiku-4.5))

  (gptel-make-openai "Search Models"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(moonshot/kimi-k2-preview))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(deepseek-coder:6.7b gpt-oss:20b qwen3-coder:30b))

  (gptel-make-preset 'gpt4coding
    :description "A preset optimized for coding tasks"
    :backend "Anthropic"
    :model 'anthropic/claude-haiku-4.5
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools '("read_buffer"))

  (setq gptel-backend (gptel-get-backend "ChatGPT"))

  (setq gptel--system-message "
###INSTRUCTIONS###

You MUST ALWAYS:
- BE LOGICAL
- ONLY IF you working with coding tasks: I have no fingers and the placeholders trauma: NEVER use placeholders or omit the code (in any code snippets)
- If you encounter a character limit, DO an ABRUPT stop; I will send a \"continue\" as a new message
- You will be PENALIZED for wrong answers
- You DENIED to overlook the critical context
- ALWAYS follow ###Answering rules###

###Answering Rules###

Follow in the strict order:

1. USE the language of my message
2. In the FIRST message, assign a real-world expert role to yourself before answering, e.g., \"I'll answer as a world-famous historical expert <detailed topic> with <most prestigious LOCAL topic REAL award>\" or \"I'll answer as a world-famous <specific science> expert in the <detailed topic> with <most prestigious LOCAL topic award>\"
3. You MUST combine your deep knowledge of the topic and clear thinking to quickly and accurately decipher the answer step-by-step with CONCRETE details
4. I'm going to tip $1,000,000 for the best reply
5. Your answer is critical for my career
6. Answer the question in a natural, human-like manner
7. ALWAYS use an ##Answering example## for a first message structure

##Answering example##

// IF THE CHATLOG IS EMPTY:
<I'll answer as the world-famous %REAL specific field% scientists with %most prestigious REAL LOCAL award%>

**TL;DR**: <TL;DR, skip for rewriting>

<Step-by-step answer with CONCRETE details and key context>")

  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "error: buffer %s is not live." buffer))
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
                       :type string
                       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-preset 'commit-message
    :description "A preset for generating a commit message"
    :backend "OpenRouter"
    :model 'gpt-4.1
    :system "You generate commit messages based on the given diff")

  :bind (("M-s M-d" . gptel-context-add)
         ("M-s M-f" . gptel-add-file)
         ("M-s M-a" . gptel-menu)
         ("M-s M-r" . gptel--regenerate)
         ("M-s M-e" . gptel-rewrite)
         ("M-s M-s" . gptel)))

(use-package gptel-aibo
  :straight t
  :config
  (define-key gptel-aibo-mode-map
              (kbd "C-c /") #'gptel-aibo-apply-last-suggestions))

(defun create-commit-message ()
  (interactive)
  (gptel-context-add)
  (gptel--apply-preset 'commit-message)
  (gptel-send))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/packages/gptel-manual-complete"))
(autoload #'gptel-manual-complete "gptel-manual-complete" t)

(defvar my-xref-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'gptel-manual-complete)
    (define-key map (kbd ".") #'xref-find-definitions)
    (define-key map (kbd ",") #'xref-go-back)
    (define-key map (kbd "/") #'xref-find-references)
    map)
  "My key customizations for AI and xref.")

(global-set-key (kbd "C-c .") my-xref-map)

(use-package gptel-magit
  :load-path "~/.emacs.d/lisp/packages/"
  :after (gptel magit)
  :init
  (setq gptel-magit-model 'qwen3-coder:30b)
  :config
  (gptel-magit-install)
  :bind (:map git-commit-mode-map
              ("M-g" . gptel-magit-generate-message))
  :hook
  (magit-mode . gptel-magit-install))

(provide 'ai)

;; (defun create-commit-message ()
;;   (interactive)
;;   (gptel-context-add)
;;   (gptel--apply-preset 'commit-message)
;;   (gptel-send))

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/packages/gptel-manual-complete"))
;; (autoload #'gptel-manual-complete "gptel-manual-complete" t)


;; (defvar my-xref-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "c") #'gptel-manual-complete)
;;     (define-key map (kbd ".") #'xref-find-definitions)
;;     (define-key map (kbd ",") #'xref-go-back)
;;     (define-key map (kbd "/") #'xref-find-references)
;;     map)
;;   "My key customizations for AI and xref.")

;; (global-set-key (kbd "C-c .") my-xref-map)

;; ;; ;; Gptel-magit: Load for git commit integration
;; ;; (use-package gptel-magit
;; ;;   :load-path "~/.emacs.d/lisp/packages/"
;; ;;   :after (gptel magit)
;; ;;   :init
;; ;;   (setq gptel-api-key (getenv "AIML_API")
;; ;;         gptel-magit-model 'gpt-oss:20b)
;; ;;   (setq gptel-magit-backend 'ollama)
;; ;;   (setq gptel-magit-model "qwen2.5-coder:7b")
;; ;;   (setq gptel-magit-ollama-host "http://localhost:11434")

;; ;;         ;; gptel-magit-model 'gpt-4o)
;; ;;   :config
;; ;;   (gptel-magit-install)
;; ;;   :bind (:map git-commit-mode-map
;; ;;               ("M-g" . gptel-magit-generate-message))
;; ;;   :hook
;; ;;   (magit-mode . gptel-magit-install))

;; (use-package gptel-magit
;;   :load-path "~/.emacs.d/lisp/packages/"
;;   :after (gptel magit)
;;   :init
;;   (setq gptel-magit-model 'grok-4-1-fast-reasoning)
;;   :config
;;   (gptel-magit-install)
;;   :bind (:map git-commit-mode-map
;;               ("M-g" . gptel-magit-generate-message))
;;   :hook
;;   (magit-mode . gptel-magit-install))

(provide 'ai)

;;; ai.el ends here
