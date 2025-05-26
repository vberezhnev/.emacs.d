;; (use-package gptel
;;   ;; :ensure t
;;   :quelpa (gptel
;; 					 :fetcher github
;; 					 :repo "karthink/gptel"
;; 					 :branch "master")
;;   :init
;;   (setq gptel-api-key (getenv "AIML_API"))
;;   (setq gptel-max-tokens 8024)
;;   :config
;;   (setq gptel-model 'gpt-4o
;;         gptel-backend
;;         (gptel-make-openai "AIMLAPI"
;;           :host "api.aimlapi.com"
;;           :endpoint "/chat/completions"
;;           :stream t
;;           :key gptel-api-key
;;           :models '(gpt-4o
;;                     gpt-4o-2024-08-06
;;                     gpt-4-turbo
;;                     chatgpt-4o-latest)))
;;   :bind (("M-s M-d" . gptel-context-add)
;;          ("M-s M-f" . gptel-add-file)
;;          ("M-s M-a" . gptel-menu)
;;          ("M-s M-r" . gptel--regenerate)
;;          ("M-s M-e" . gptel-rewrite)
;;          ("M-s M-s" . gptel)))

(use-package whisper
  :load-path "~/.emacs.d/lisp/packages/whisper.el"
  :bind ("M-s M-t" . whisper-run)
  :config
  (setq whisper-install-directory "/tmp/"
        whisper-model "base"
        ;; whisper-model "base"
        whisper-language "ru"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)
        whisper-enable-speed-up nil
        whisper-recording-timeout 300))
