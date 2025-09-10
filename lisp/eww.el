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
                             :height 1.5))
  (add-hook 'eww-mode-hook 'my-eww-font-setup)

  (setq eww-search-prefix "http://localhost/search?q=")
  
  ;; Включение отображения изображений
  (setq eww-retrieve-command '("wget" "--quiet" "-O" "-")
        shr-use-fonts t  ;; Использовать шрифты с переменной шириной
        shr-image-display t)  ;; Показывать изображения

  ;; Улучшение читаемости
  (setq shr-width 80  ;; Ширина текста (в символах)
        shr-discard-aria-hidden t  ;; Игнорировать скрытые элементы
        shr-use-xbuttons nil  ;; Не использовать кнопки для ссылок
        eww-header-line-format "%t")  ;; Показывать заголовок в заголовочной строке

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
