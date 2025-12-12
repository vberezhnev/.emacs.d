(use-package elysium
  :straight t
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

;; (use-package gptel-watch
;;   :straight (:host github :repo "ISouthRain/gptel-watch" :branch "master")
;;   :after gptel
;;   :config
;;   (gptel-watch-global-mode 1))

(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :demand t)

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :branch "master")
  :demand t
  :init
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all))

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
    :models '(grok-code-fast-1))

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
    :models '(qwen2.5:7b deepseek-coder:6.7b gpt-oss:20b))

  (gptel-make-tool
   :name "read_buffer"                    ; javascript-style snake_case name
   :function (lambda (buffer)                  ; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
		 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
		 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
		       :type string            ; :type value must be a symbol
		       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")                     ; An arbitrary label for grouping

  (gptel-make-preset 'gpt4coding
    :description "A preset optimized for coding tasks"
    :backend "Claude"
    :model 'openai/gpt-5-1-codex-mini
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools '("read_buffer")) ;gptel tools or tool names

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
3. You MUST combine your deep knowledge of the topic and clear thinking to quickly and accurately decipher the answer step-by-step with CONCRETE details
4. I'm going to tip $1,000,000 for the best reply
5. Your answer is critical for my career
6. Answer the question in a natural, human-like manner
7. ALWAYS use an ##Answering example## for a first message structure

##Answering example##

// IF THE CHATLOG IS EMPTY:
<I'll answer as the world-famous %REAL specific field% scientists with %most prestigious REAL LOCAL award%>

**TL;DR**: <TL;DR, skip for rewriting>

<Step-by-step answer with CONCRETE details and key context>")

  :bind (("M-s M-d" . gptel-context-add)
	 ("M-s M-f" . gptel-add-file)
	 ("M-s M-a" . gptel-menu)
	 ("M-s M-r" . gptel--regenerate)
	 ("M-s M-e" . gptel-rewrite)
	 ("M-s M-s" . gptel)))

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

;; Gptel-magit: Load for git commit integration
(use-package gptel-magit
  :load-path "~/.emacs.d/lisp/packages/"
  :after (gptel magit)
  :init
  (setq gptel-api-key (getenv "AIML_API")
        gptel-magit-model 'gpt-4o)
  :config
  (gptel-magit-install)
  :bind (:map git-commit-mode-map
              ("M-g" . gptel-magit-generate-message))
  :hook
  (magit-mode . gptel-magit-install))

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
