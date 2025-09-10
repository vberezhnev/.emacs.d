(use-package grid
  :straight (:host github
             :repo "ichernyshovvv/grid.el"
             :branch "master"))

(require 'grid)


(defun my/enlight-disable-modeline-hook ()
  "Disable modeline in enlight buffer."
  (when (string-equal (buffer-name) "*enlight*")
    (my/toggle-modeline)))

(add-hook 'enlight-mode-hook 'my/enlight-disable-modeline-hook)

(defvar enlight-lipsum
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")

(defface enlight-yellow-bold
  '((t (:foreground "spring green" :bold t)))
  "Yellow bold face")

;; (defvar enlight-logo
;;   (propertize
;;    "    ___           ___           ___           ___           ___                 \n\
;;    /  /\\         /__/\\         /  /\\         /  /\\         /  /\\    \n\
;;   /  /:/_       |  |::\\       /  /::\\       /  /:/        /  /:/_   \n\
;;  /  /:/ /\\      |  |:|:\\     /  /:/\\:\\     /  /:/        /  /:/ /\\  \n\
;; /  /:/ /:/_   __|__|:|\\:\\   /  /:/~/::\\   /  /:/  ___   /  /:/ /::\\ \n\
;; /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ /:/\\:\\ /__/:/  /  /\\ /__/:/ /:/\\:\\\n\
;; \\  \\:\\/:/ /:/ \\  \\:\\~~\\__\\/ \\  \\:\\/:/__\\/ \\  \\:\\ /  /:/ \\  \\:\\/:/~/:/\n\
;;  \\  \\::/ /:/   \\  \\:\\        \\  \\::/       \\  \\:\\  /:/   \\  \\::/ /:/ \n\
;;   \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \\:\\/:/     \\__\\/ /:/  \n\
;;    \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \\::/        /__/:/   \n\
;;     \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/    "
;;    'face 'enlight-yellow-bold))

;; (defun enlight-insert-logo ()
;;   "Insert centered Emacs logo image from ~/.emacs.d/images/Emacs-logo.svg using grid.el."
;;   (let* ((image-file (expand-file-name "~/.emacs.d/images/Emacs-logo.svg"))
;;          ;; Получаем размеры окна в символах
;;          (window-width (window-body-width))
;;          (window-height (window-body-height))
;;          ;; Проверяем поддержку SVG
;;          (svg-supported (image-type-available-p 'svg))
;;          ;; Создаем изображение, если поддерживается SVG
;;          (image (when svg-supported
;;                   (create-image image-file 'svg nil :scale 1.0)))
;;          ;; Альтернативный текст, если изображение недоступно
;;          (fallback-text (propertize " [Emacs Logo] " 'face 'enlight-yellow-bold))
;;          ;; Выбираем содержимое для отображения
;;          (content (if (and svg-supported image)
;;                       (propertize " " 'display image)
;;                     fallback-text))
;;          ;; Получаем размеры изображения (или текста, если используется запасной вариант)
;;          (image-props (if (and svg-supported image)
;;                           (image-size image t)
;;                         (cons (length fallback-text) 1)))
;;          (image-width (car image-props))
;;          (image-height (cdr image-props))
;;          ;; Переводим размеры в символы
;;          (char-width (ceiling (/ image-width (float (frame-char-width)))))
;;          (char-height (ceiling (/ image-height (float (frame-char-height)))))
;;          ;; Вычисляем вертикальный отступ
;;          (vertical-padding (max 0 (floor (/ (- window-height char-height) 2)))))
;;     ;; Отладочный вывод
;;     (message "Inserting logo: file=%s, svg-supported=%s, window-width=%d, window-height=%d, image-width=%.2f, image-height=%.2f, char-width=%d, char-height=%d, vertical-padding=%d"
;;              image-file svg-supported window-width window-height image-width image-height char-width char-height vertical-padding)
;;     ;; Очищаем буфер перед вставкой
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (goto-char (point-min))
;;       ;; Вставляем верхний вертикальный отступ
;;       (insert (make-string vertical-padding ?\n))
;;       ;; Вставляем изображение (или текст) с центрированием через grid.el
;;       (grid-insert-box
;;        `(:content ,content
;;          :width ,window-width
;;          :align center
;;          :padding 0))
;;       ;; Вставляем нижний вертикальный отступ
;;       (insert (make-string vertical-padding ?\n))
;;       ;; Проверяем, был ли файл найден
;;       (unless (or svg-supported (file-exists-p image-file))
;;         (grid-insert-box
;;          `(:content ,(format "Error: Emacs-logo.svg not found at %s" image-file)
;;            :width ,window-width
;;            :align center
;;            :padding 0))))))

;; (defun enlight-insert-logo-hook ()
;;   "Insert logo in enlight buffer after content."
;;   (when (string-equal (buffer-name) "*enlight*")
;;     (let ((inhibit-read-only t))
;;       (erase-buffer) ;; Очищаем буфер
;;       (goto-char (point-min))
;;       (enlight-insert-logo))))

(defvar spacing
  (propertize
    "\n            хачумба            \n
\n                               \n
\n                               \n
\n                               \n
\n                               \n
\n                               \n
\n                               \n
\n                               "
   'face 'enlight-yellow-bold))

(use-package enlight
  :straight (:host github
             :repo "ichernyshovvv/enlight"
             :branch "master")
  :custom
  ;; (enlight-content
  ;;  (concat
  ;;   (with-temp-buffer
  ;;     (insert-image (create-image "~/.emacs.d/images/black-hole.png"))
  ;;     (buffer-string))
  ;;   ;; (grid-get-box `(:content ,enlight-logo :width 80 :align center))
  ;;   "\n\n"
  ;;   (grid-get-row
  ;;    (list
  ;;     (grid-get-box
  ;;      `(:content
  ;;        ,(enlight-menu
  ;;          '(("Menu"
  ;;             (" Recently opened files" recentf "r")
  ;;             (" Jump to bookmark" bookmark-jump "b")
  ;;             (" Open project" project-switch-project "p")
  ;;             (" Open org-agenda" (org-agenda nil "a") "a"))))
  ;;        :width 80 :align center))))
  ;;   ))

  ;; (enlight-content
  ;;  (concat
  ;;   ;; (with-temp-buffer
  ;;   ;;   (insert-image (create-image "~/.emacs.d/images/Emacs-logo.svg")) ;; "          "
  ;;   ;;   (buffer-string))
  ;;   ;; (grid-get-box `(:content ,spacing :width 80 :align center))
  ;;   ;; "\n            хачумба            "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ;; "\n                               "
  ;;   ))

  (enlight-content
   (let* ((image (create-image "~/.emacs.d/images/Emacs-logo.svg"))
          (size (image-size image)))
     (with-temp-buffer
       (insert-image image
                     (concat (make-string (round (car size)) ?\s)
                             (make-string (round (cdr size)) ?\n)))
       (buffer-string))
     )
     ;; "хачумба"
   )
  :config
  ;; (add-hook 'enlight-after-insert-hook 'enlight-insert-logo-hook)
  (enlight-open)
  (my/toggle-cursor))

(setopt initial-buffer-choice #'enlight)
