;; (use-package eww-history-ext
;;   :straight (:host github :repo "1History/eww-history-ext" :branch "master")
;;   :custom ((eww-history-ext-elfeed-integration t))
;;   :config
;;   (eww-history-ext-enable))

;; ;; Set SOCKS5 proxy with authentication
;; (setq socks-server '("Default server" "186.179.50.210" 8000 5 "jP6tA6" "xavW6X"))
;; (setq socks-noproxy '("127.0.0.1")) ; Exclude localhost from proxying
;; (setq url-gateway-method 'socks)

;; ;; Fix for HTTPS over SOCKS5 (for Emacs >= 25)
;; (defadvice url-http (around url-http-around activate)
;;   "Fix `url-gateway-method' bug for HTTPS over SOCKS."
;;   (if (eq 'socks url-gateway-method)
;;       (let ((gateway-method url-gateway-method))
;;         ad-do-it)
;;     ad-do-it))


(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "=") #'text-scale-increase)
  (define-key eww-mode-map (kbd "-") #'text-scale-decrease)
  (define-key eww-mode-map (kbd "0") #'text-scale-adjust))

(use-package eww
  :ensure nil  ;; EWW встроен в Emacs
  :custom
  (eww-auto-rename-buffer 'title)
  :config
  (add-hook 'eww-mode-hook #'(lambda () (blink-cursor-mode -1)))

  (define-advice eww (:around (oldfun &rest args) always-new-buffer)
    "Always open EWW in a new buffer."
    (let ((current-prefix-arg '(4)))
      (apply oldfun args)))

  (defun my-eww-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.2))
  (add-hook 'eww-mode-hook 'my-eww-font-setup)

  (setq eww-search-prefix "http://localhost:80/search?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads/"))
  ;; (setq eww-search-prefix "https://duckduckgo.com/search?q=")
  ;; (setq eww-search-prefix "https://priv.au/search?q=")
  ;; (setq eww-search-prefix "https://searx.oloke.xyz/search?q=")

  (setq shr-image-animate t)

  ;; Включение отображения изображений
  (setq eww-retrieve-command '("wget" "--quiet" "-O" "-")
        shr-use-fonts t  ;; Использовать шрифты с переменной шириной
        shr-image-display t)  ;; Показывать изображения

  ;; Улучшение читаемости
  (setq ;; shr-width 280  ;; Ширина текста (в символах)
        shr-discard-aria-hidden t  ;; Игнорировать скрытые элементы
        shr-use-xbuttons t  ;; Не использовать кнопки для ссылок
        eww-header-line-format "EWW  - %t")  ;; Показывать заголовок в заголовочной строке


  ;; ;; Keybinds
  ;; (with-eval-after-load 'eww
  ;;   (define-key eww-mode-map (kbd "b") #'eww-back-url)
  ;;   (define-key eww-mode-map (kbd "a") #'eww-add-bookmark)
  ;;   (define-key eww-mode-map (kbd "U") #'shr-copy-url)
  ;;   (define-key eww-mode-map (kbd "D") #'my/eww-download-image-at-point))

  ;; Привязки клавиш для Evil mode
  (with-eval-after-load 'evil
    (evil-define-key 'normal eww-mode-map
      ;; Новые привязки, избегающие конфликтов с Vim
      (kbd "g r") 'eww-reload           ;; Обновить страницу (вместо r)
      (kbd "g e") 'eww-browse-with-external-browser           ;; Открыть в браузере
      (kbd "g b") 'eww-back             ;; Назад (вместо b)
      (kbd "g f") 'eww-forward          ;; Вперед (вместо f)
      (kbd "g t") 'eww                  ;; Открыть новую "вкладку" (вместо t)
      (kbd "g l") 'eww-follow-link      ;; Перейти по ссылке (вместо l)
      (kbd "g u") 'eww-copy-page-url    ;; Копировать URL страницы (вместо u)
      (kbd "g R") 'eww-readable         ;; Режим чтения (вместо R)
      (kbd "g B") 'eww-back-url         ;; Назад по URL (вместо B)
      (kbd "g F") 'eww-forward-url      ;; Вперед по URL (вместо F)
      (kbd "C-c t") 'eww                ;; Новая вкладка (URL) (вместо C-t)
      (kbd "C-c w") 'kill-this-buffer   ;; Закрыть вкладку (вместо C-w)
      (kbd "C-c n") 'eww-next-url       ;; Следующая страница (вместо C-n)
      (kbd "C-c p") 'eww-previous-url   ;; Предыдущая страница (вместо C-p)
      (kbd "g h") 'eww-list-histories   ;; Показать историю (вместо H)
      ))

  (setq eww-history-limit 1000))

(use-package language-detection
  :straight t)

(require 'cl-lib)

(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))


;; (setq eww-search-prefix "https://searx.labrynth.org/search?q=")
;; (setq eww-download-directory (expand-file-name "~/Downloads/"))

;; (defun my-browse-url-mpv (url &rest _args)
;;   "Open URL in mpv."
;;   (start-process "mpv" nil "mpv" url))

;; (defun my-browse-url-pdf (url &rest _args)
;;   "Fetch remote PDF and open in pdf-tools within Emacs."
;;   (let ((tmp (make-temp-file "emacs-pdf-" nil ".pdf")))
;;     (url-copy-file url tmp t)
;;     (find-file-other-window tmp)
;;     (pdf-view-mode)))

;; (setq browse-url-handlers
;;       '(("\\(youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\|twitch\\.tv\\)" . my-browse-url-mpv)
;;         ("\\.mp4$" . my-browse-url-mpv)
;;         ("\\.pdf$" . my-browse-url-pdf)
;;         ("^gemini://" . elpher-browse-url-elpher)
;;         ("^gopher://" . elpher-browse-url-elpher)
;;         ("." . eww-browse-url)))

;; ;; Keep your fallback setting
;; (setq browse-url-secondary-browser-function 'browse-url-generic
;;       browse-url-generic-program "chromium")

;; (setq shr-width 100)
;; (setq shr-max-width 120)
;; (setq shr-indentation 4)

;; (setq shr-use-fonts nil)
;; (setq shr-max-image-size '(800 . 600))

;; (defun my/eww-download-image-at-point ()
;;   "Download image at point to `eww-download-directory'."
;;   (interactive)
;;   (let ((url (or (get-text-property (point) 'image-url)
;;                  (get-text-property (point) 'shr-url))))
;;     (if (not url)
;;         (message "No image at point")
;;       (let* ((filename (file-name-nondirectory (url-filename (url-generic-parse-url url))))
;;              (dest (expand-file-name filename eww-download-directory)))
;;         (url-copy-file url dest t)
;;         (message "Saved: %s" dest)))))

;; ;; Keybinds
;; (with-eval-after-load 'eww
;;   (define-key eww-mode-map (kbd "b") #'eww-back-url)
;;   (define-key eww-mode-map (kbd "a") #'eww-add-bookmark)
;;   (define-key eww-mode-map (kbd "U") #'shr-copy-url)
;;   (define-key eww-mode-map (kbd "D") #'my/eww-download-image-at-point))

;; (provide 'browser)
