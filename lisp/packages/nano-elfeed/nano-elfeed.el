;;; nano-elfeed.el --- NANO elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/nano-elfeed
;; Keywords: news
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (elfeed) (nano-theme) (relative-date))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require 's)
(require 'stripes)
(require 'hl-line)
(require 'relative-date)
(require 'elfeed)
(require 'elfeed-org)
(require 'nano-theme)

;; (setq-local line-spacing 0)
;; (add-hook 'elfeed-search-mode-hook (lambda () (setq-local line-spacing 0)))

(defconst nano-elfeed--rss-icon-data
"<svg fill=\"#000000\" height=\"256px\" width=\"256px\" version=\"1.1\" id=\"Layer_1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"-271 273 256 256\" xml:space=\"preserve\"><g id=\"SVGRepo_bgCarrier\" stroke-width=\"0\"></g><g id=\"SVGRepo_tracerCarrier\" stroke-linecap=\"round\" stroke-linejoin=\"round\"></g><g id=\"SVGRepo_iconCarrier\"> <g> <path d=\"M-271,360v48.9c31.9,0,62.1,12.6,84.7,35.2c22.6,22.6,35.1,52.8,35.1,84.8v0.1h49.1c0-46.6-19-88.7-49.6-119.4 C-182.2,379-224.4,360.1-271,360z\"></path> <path d=\"M-237,460.9c-9.4,0-17.8,3.8-24,10s-10,14.6-10,24c0,9.3,3.8,17.7,10,23.9c6.2,6.1,14.6,9.9,24,9.9s17.8-3.7,24-9.9 s10-14.6,10-23.9c0-9.4-3.8-17.8-10-24C-219.2,464.7-227.6,460.9-237,460.9z\"></path> <path d=\"M-90.1,348.1c-46.3-46.4-110.2-75.1-180.8-75.1v48.9C-156.8,322-64.1,414.9-64,529h49C-15,458.4-43.7,394.5-90.1,348.1z\"></path> </g> </g></svg>")

(defun nano-elfeed--make-icon (image)
  (let* ((img-width (car (image-size image t)))
         (img-height (cdr (image-size image t)))
         (ch (frame-char-height))
         (cw (frame-char-width))
         (icon-height (+ 0 (* 2 ch)))
         (char-width  (+ 1 (truncate (/ (* 2 ch) cw))))
         (icon-width  (* char-width cw))
         (scale (/ (float icon-height) (float img-height)))
         (scaled-width (truncate (* scale img-width)))
         (scaled-height (truncate (* scale img-height)))
         (icon-true-width (truncate (* img-width scale)))
         (margin (max 0 (- icon-width icon-true-width)))
         (icon-width (+ icon-width (% margin 2)))
         (margin (- margin (% margin 2)))
         (thumbnail (cons (car image) (cl-copy-list (cdr image)))))
    (message "char-height: %d, frame-char-height: %d, icon-height: %d"
         (face-attribute 'default :height)
         (frame-char-height)
         icon-height)


    (plist-put (cdr thumbnail) :height scaled-height)
    (plist-put (cdr thumbnail) :width  scaled-width)
    (plist-put (cdr thumbnail) :margin (cons (/ margin 2) 0))
    (plist-put (cdr thumbnail) :ascent 80)
    (cons (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  0 icon-width ch) thumbnail)
                                  ;; 'line-height t
				  )
          (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  ch icon-width ch) thumbnail)
                                  ;; 'line-height t
				  ))
    ;; (plist-put (cdr thumbnail) :ascent 'center)
    ))

(defvar nano-elfeed-icon-path "~/.emacs.d/lisp/packages/nano-elfeed/icons/")

(defun nano-elfeed-make-icon (name)
  (let* ((image-unread (create-image (format "%s/%s-unread.svg" nano-elfeed-icon-path name)))
         (image-unread (nano-elfeed--make-icon image-unread))
         (image-read (create-image (format "%s/%s-read.svg" nano-elfeed-icon-path name)))
         (image-read (nano-elfeed--make-icon image-read)))
    (cons image-unread image-read)))

;; (defvar nano-elfeed-icons
;;   `(("RSS"             . ,(nano-elfeed-make-icon "rss"))
;;     ("Aeon"            . ,(nano-elfeed-make-icon "aeon"))
;;     ("YouTube"         . ,(nano-elfeed-make-icon "youtube"))
;;     ("Emacs"           . ,(nano-elfeed-make-icon "reddit"))
;;     ("Emacs org-mode"  . ,(nano-elfeed-make-icon "reddit"))
;;     ("Slashdot"        . ,(nano-elfeed-make-icon "slashdot"))
;;     ("Ars Technica"    . ,(nano-elfeed-make-icon "ars-technica"))
;;     ("Boing Boing"     . ,(nano-elfeed-make-icon "boing-boing"))
;;     ("eLife"           . ,(nano-elfeed-make-icon "elife"))
;;     ("Plos Comp.Bio"   . ,(nano-elfeed-make-icon "plos"))
;;     ("Quanta"          . ,(nano-elfeed-make-icon "quanta"))
;;     ("BioRxiv Neuro"   . ,(nano-elfeed-make-icon "biorxiv"))

;;     ("Ivan Limarev - Telegram"        . ,(nano-elfeed-make-icon "telegram"))
;;     ("Записки продакт-менеджера | Вадим Кузнецов - Telegram"        . ,(nano-elfeed-make-icon "telegram"))
;;     ("Осознанная Меркантильность - Telegram"        . ,(nano-elfeed-make-icon "telegram"))
;;     ("Москвач Бизнес - Telegram"        . ,(nano-elfeed-make-icon "telegram"))
;;     ("Nonabstractum | Экономика & Стратегия - Telegram"        . ,(nano-elfeed-make-icon "telegram"))

;;     ("Rust - Хабр"     . ,(nano-elfeed-make-icon "habr"))
;;     ("The Rust Programming Language - Reddit" . ,(nano-elfeed-make-icon "reddit"))))

(defvar nano-elfeed-icons
  `(("RSS"             . ,(or (nano-elfeed-make-icon "rss") (cons "RSS" "RSS")))
    ("Aeon"            . ,(or (nano-elfeed-make-icon "aeon") (cons "Aeon" "Aeon")))
    ("YouTube"         . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("Emacs"           . ,(or (nano-elfeed-make-icon "reddit") (cons "Emacs" "Emacs")))
    ("Emacs org-mode"  . ,(or (nano-elfeed-make-icon "reddit") (cons "Emacs org-mode" "Emacs org-mode")))
    ("Slashdot"        . ,(or (nano-elfeed-make-icon "slashdot") (cons "Slashdot" "Slashdot")))
    ("Ars Technica"    . ,(or (nano-elfeed-make-icon "ars-technica") (cons "Ars Technica" "Ars Technica")))
    ("Boing Boing"     . ,(or (nano-elfeed-make-icon "boing-boing") (cons "Boing Boing" "Boing Boing")))
    ("eLife"           . ,(or (nano-elfeed-make-icon "elife") (cons "eLife" "eLife")))
    ("Plos Comp.Bio"   . ,(or (nano-elfeed-make-icon "plos") (cons "Plos Comp.Bio" "Plos Comp.Bio")))
    ("Quanta"          . ,(or (nano-elfeed-make-icon "quanta") (cons "Quanta" "Quanta")))
    ("BioRxiv Neuro"   . ,(or (nano-elfeed-make-icon "biorxiv") (cons "BioRxiv Neuro" "BioRxiv Neuro")))
    ("Telegram"        . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Reddit"          . ,(or (nano-elfeed-make-icon "reddit") (cons "Reddit" "Reddit")))
    ("Habr"            . ,(or (nano-elfeed-make-icon "habr") (cons "Habr" "Habr")))
    ("Ivan Limarev - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Записки продакт-менеджера | Вадим Кузнецов - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Осознанная Меркантильность - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Москвач Бизнес - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Nonabstractum | Экономика & Стратегия - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Август Круглый Год - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Rust - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Go in Action - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Machinelearning - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Машинное обучение RU - Telegram" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("RSS-Bridge" . ,(or (nano-elfeed-make-icon "telegram") (cons "Telegram" "Telegram")))
    ("Курс по самодисциплине от Глеба Карпова" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("Andrii Baumeister" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("The Rust Programming Language - Reddit" . ,(or (nano-elfeed-make-icon "reddit") (cons "Reddit" "Reddit")))
    ("Rust - Хабр" . ,(or (nano-elfeed-make-icon "habr") (cons "Habr" "Habr")))
    ("Научно-популярное - Хабр" . ,(or (nano-elfeed-make-icon "habr") (cons "Habr" "Habr")))
    ("Астрономия - Хабр" . ,(or (nano-elfeed-make-icon "habr") (cons "Habr" "Habr")))
    ;; ("Space.com" . ,(or (nano-elfeed-make-icon "rss") (cons "RSS" "RSS")))
    ("Rust Blog" . ,(or (nano-elfeed-make-icon "rss") (cons "RSS" "RSS")))
    ("Terminally Incoherent" . ,(or (nano-elfeed-make-icon "rss") (cons "RSS" "RSS")))
    ("null program" . ,(or (nano-elfeed-make-icon "rss") (cons "RSS" "RSS")))
    ("Alexey Arestovych" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("Let's Get Rusty" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("No Boilerplate" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("Vert Dider" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))
    ("Lex Fridman" . ,(or (nano-elfeed-make-icon "youtube") (cons "YouTube" "YouTube")))))

(defun nano-elfeed-get-icon (name unread)
  (let ((icon (alist-get (s-trim name) nano-elfeed-icons nil nil #'equal)))
    (when icon (if unread (car icon) (cdr icon)))))
  
(defvar nano-elfeed-rss-icon-active
      (nano-elfeed--make-icon
       (create-image (format nano-elfeed--rss-icon-data "orange") 'svg t)))

(defvar nano-elfeed-rss-icon-inactive
      (nano-elfeed--make-icon
       (create-image (format nano-elfeed--rss-icon-data "#90A4AE") 'svg t)))

(defun nano-elfeed-entry (title subtitle date unread tags &optional no-newline)
  "Display an elfeed entry with title, subtitle, date, unread status, and tags."
  (let* ((icon (or (nano-elfeed-get-icon title unread)
                   (if unread
                       nano-elfeed-rss-icon-active
                     nano-elfeed-rss-icon-inactive)))
         (date (relative-date date))
         (subtitle (s-truncate (truncate (* 1 (window-width))) subtitle "…"))
         (tags-str (if tags
                       (concat " [" (mapconcat #'symbol-name tags ", ") "]")
                     ""))
         (foreground-color (if unread
                               (face-foreground 'default)
                             (face-foreground 'font-lock-comment-face nil t)))
         (background-color (face-background 'highlight))
         (border-color (face-background 'default))
         (face-upper `(:foreground ,foreground-color
                       :background ,background-color
                       :overline ,border-color))
         (face-title `(:foreground ,foreground-color
                       :background ,background-color
                       :weight ,(face-attribute 'bold :weight)
                       :overline ,border-color))
         (face-subtitle `(:foreground ,foreground-color
                          :background ,background-color
                          :family "Roboto Condensed"
                          :height 160
                          :underline nil))
         (face-tags `(:foreground ,foreground-color
                      :background ,background-color
                      :family "Roboto Condensed"
                      :height 140
                      :underline nil))
         (face-lower `(:foreground ,foreground-color
                       :background ,background-color
                       :underline nil)))
    (insert (concat
             ;; Upper part
             (propertize " " 'face face-upper 'display '(raise 0.5))
             (propertize (car icon) 'face face-upper)
             (propertize " " 'face face-upper)
             (propertize title 'face face-title 'elfeed-entry t)
             (propertize " " 'face face-upper
                         'display `(space :align-to (- right ,(length date) 2)))
             (propertize date 'face face-upper)
             (propertize " " 'face face-upper
                         'display '(space :align-to (- right (0))))
             (propertize " " 'display "\n")
             ;; Lower part
             (propertize " " 'face face-lower 'display '(raise -0.5))
             (propertize (cdr icon) 'face face-lower)
             (propertize " " 'face face-lower)
             (propertize subtitle 'face face-subtitle)
             (propertize tags-str 'face face-tags)
             (propertize " " 'face face-lower
                         'display '(space :align-to (- right (0))))
             (unless no-newline
               (propertize "\n"))))))

(defun nano-elfeed-search-print-entry (entry)
  "Alternative printing of elfeed entries using SVG tags and displaying tags."
  (let* ((date (elfeed-entry-date entry))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (unread (member 'unread (elfeed-entry-tags entry)))
         (tags (elfeed-entry-tags entry))  ; Extract tags
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed
                       (or (elfeed-meta feed :title)  ; Use :TITLE property if available
                           (elfeed-feed-title feed)
                           ""))))
    (nano-elfeed-entry feed-title title date unread tags t)))


;; (defun nano-elfeed-search-print-entry (entry)
;;   "Alternative printing of elfeed entries using SVG tags."
  
;;   (let* ((date (elfeed-entry-date entry))
;;          (title (or (elfeed-meta entry :title)
;;                     (elfeed-entry-title entry) ""))
;;          (unread (member 'unread (elfeed-entry-tags entry)))
;;          (feed (elfeed-entry-feed entry))
;;          (feed-title (when feed
;;                        (or (elfeed-meta feed :title)
;;                            (elfeed-feed-title feed)))))

;;     (nano-elfeed-entry feed-title title date unread t)))

(defun nano-elfeed-search-mode ()
  (setq left-fringe-width 1
        right-fringe-width 1
        left-margin-width 0
        right-margin-width 0
        stripes-unit 1)
  (set-window-buffer nil (current-buffer))

  (setq stripes-overlay-priority 50)
  (stripes-mode 1)
  (setq hl-line-overlay-priority 100)
  (hl-line-mode -1)
  (setq cursor-type nil)
  (face-remap-add-relative 'hl-line :inherit 'nano-faded-i)
  (hl-line-mode t)
  )

(defun nano-elfeed-show-mode ()
  (visual-line-mode)
 (setq truncate-lines nil)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-width 79)
    ;; (setq header-line-format nil)
    (face-remap-set-base 'default '(:height 140))
    (set-buffer-modified-p nil)))

(defun nano-elfeed-next-entry ()
  (interactive)
  (text-property-search-forward 'elfeed-entry t))

(defun nano-elfeed-prev-entry ()
  (interactive)
  (text-property-search-backward 'elfeed-entry t))

(defun nano-elfeed-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry
      (nano-elfeed-next-entry))
    (call-interactively #'elfeed-search-show-entry)))

(defun nano-elfeed-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry (forward-line 1))
    (nano-elfeed-prev-entry)
    (call-interactively #'elfeed-search-show-entry)))

(setq elfeed-search-filter "@3-days-ago +unread"          
      elfeed-search-print-entry-function
           #'nano-elfeed-search-print-entry)

(bind-key "<down>" #'nano-elfeed-next-entry 'elfeed-search-mode-map)
(bind-key "n" #'nano-elfeed-next-entry 'elfeed-search-mode-map)

(bind-key "<up>" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)
(bind-key "p" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)

(bind-key "p" #'nano-elfeed-prev-next 'elfeed-show-mode-map)
(bind-key "n" #'nano-elfeed-show-next 'elfeed-show-mode-map)

(add-hook 'elfeed-search-mode-hook #'nano-elfeed-search-mode)
(add-hook 'elfeed-show-mode-hook #'nano-elfeed-show-mode)

(setq rmh-elfeed-org-files
      (list (expand-file-name "elfeed.org" user-emacs-directory)))
(elfeed-org)

(provide 'nano-elfeed)
