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

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :branch "master")
  :demand t
  :init
  (setq gptel-verbose t
	gptel-max-tokens 8024
	gptel-temperature 0.7)
  (setq gptel-api-key (getenv "AIML_API"))

  (setq gptel-model 'openai/gpt-5-1-chat-latest)
  :config
  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  (gptel-make-openai "ChatGPT"
    :host "api.aimlapi.com"
    :endpoint "/chat/completions"
    :stream t
    :key gptel-api-key
    :models '(gpt-4o openai/gpt-5-1-chat-latest openai/gpt-5-chat-latest))

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
    :models '(anthropic/claude-opus-4-5))

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

  (setq gptel-backend (gptel-get-backend "ChatGPT"))

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
		 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
		 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
		       :type "string"
		       :description "The directory where to create the file")
               '(:name "filename"
		       :type "string"
		       :description "The name of the file to create")
               '(:name "content"
		       :type "string"
		       :description "The content to write to the file"))
   :category "filesystem")

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
  ;; :after (gptel magit)
  :init
  (setq gptel-api-key (getenv "AIML_API"))
  (setq gptel-magit-model 'gpt-4o)
  :config
  ;; (setq gptel-magit-model 'gpt-4o)
  ;; (setq gptel-magit-backend
  ;;       (gptel-make-openai "AIMLAPI"
  ;;         :host "api.aimlapi.com"
  ;;         :endpoint "/chat/completions"
  ;;         :stream nil
  ;;         :key (getenv "AIML_API")
  ;;         :models '(gpt-4o)))
  (gptel-magit-install)

  :bind (:map git-commit-mode-map
	      ("M-g" . gptel-magit-generate-message))
  :hook
  (magit-mode . gptel-magit-install))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick" :branch "master")
  :bind (("C-c TAB" . gptel-quick)))

(use-package gptel-autocomplete
  :straight (:host github :repo "JDNdeveloper/gptel-autocomplete" :branch "main")
  ;; :demand t
  :init
  (setq gptel-autocomplete-before-context-lines 100
	gptel-autocomplete-after-context-lines 20
	gptel-autocomplete-temperature 0.1
	gptel-autocomplete-use-context t)
  :bind (("C-c TAB" . gptel-complete)))
