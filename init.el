;; (setq package-archives
;;       '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
;; 	("ORG"		. "https://orgmode.org/elpa/")
;;         ("MELPA Stable" . "https://stable.melpa.org/packages/")
;;         ("MELPA"        . "https://melpa.org/packages/"))
;;       package-archive-priorities
;;       '(("ORG"		. 20)
;; 	("MELPA"        . 15)
;; 	("MELPA Stable" . 10)
;;         ("GNU ELPA"     . 5)))

(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(setq package-archive-priorities '(("melpa"  . 3)
                                   ("gnu"    . 2)
                                   ("nongnu" . 1)))
(package-initialize)

;;(package-refresh-contents)

;; (require 'package)
;; ;; MELPA 20
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/") t)
;; (setcdr (assoc "melpa" package-archive-priorities) 20)

;; ;; ELPA 15
;; (add-to-list 'package-archives
;;              '("elpa" . "https://elpa.gnu.org/packages/") t)
;; (setcdr (assoc "elpa" package-archive-priorities) 15)

;; ;; NonGNU 10
;; (add-to-list 'package-archives
;;              '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
;; (setcdr (assoc "nongnu" package-archive-priorities) 10)

;; ;; Org 30
;; (add-to-list 'package-archives
;;              '("org" . "https://orgmode.org/elpa/") t)
;; (setcdr (assoc "org" package-archive-priorities) 30)

;; (package-initialize)

;; Install use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(parrot-start-animation format-all-buffer))
 '(doom-modeline-bar-width 6)
 '(doom-modeline-buffer-file-name-style 'auto)
 '(doom-modeline-buffer-name nil)
 '(doom-modeline-buffer-state-icon t)
 '(doom-modeline-height 30)
 '(doom-modeline-highlight-modified-buffer-name nil)
 '(doom-modeline-icon t)
 '(doom-modeline-project-detection 'projectile)
 '(global-wakatime-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))
 '(org-agenda-files
	 '("/home/chopin/Org/agenda/calendar.org" "/home/chopin/Org/agenda/agenda.org" "/home/chopin/Org/agenda/cal_inbox.org" "/home/chopin/Org/2Brain/2023-03-10-05:45:45-ооп_это.org" "/home/chopin/Org/2Brain/2023-02-04-16:19:33-школьные_предметы.org" "/home/chopin/Org/2Brain/2023-02-27-23:38:55-7_советов_по_очистке_кода_react.org" "/home/chopin/Org/2Brain/2023-03-05-20:41:26-latin_gender.org" "/home/chopin/Org/2Brain/2023-02-13-19:00:50-rust.org" "/home/chopin/Org/2Brain/20230202193313-bibliotheca_alexandrina.org"))
 '(org-hugo-base-dir ~/Templates/Blog)
 '(org-safe-remote-resources
	 '("\\`https://cs7\\.pikabu\\.ru/post_img/2018/12/17/7/1545041477174040899\\.jpg\\'" "\\`https://cs11\\.pikabu\\.ru/post_img/2018/12/17/7/1545041626180870887\\.jpg\\'" "\\`https://upload\\.wikimedia\\.org/wikipedia/commons/thumb/0/00/Giovanni_Stanchi%2C_Watermelons%2C_Peaches%2C_Pears%2C_and_Other_Fruit_in_a_Landscape\\.jpg/2560px-Giovanni_Stanchi%2C_Watermelons%2C_Peaches%2C_Pears%2C_and_Other_Fruit_in_a_Landscape\\.jpg\\'" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'" "\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)"))
 '(telega-active-locations-mode t)
 '(telega-appindicator-mode t)
 '(telega-appindicator-show-mentions t)
 '(telega-completing-read-function 'ido-completing-read)
 '(telega-patrons-mode t)
 '(telega-root-auto-fill-mode t)
 '(telega-root-fill-column 232)
 '(telega-sticker-animated-play nil)
 '(telega-translate-to-language-by-default "en")
 '(warning-suppress-log-types
	 '((use-package)
		 (use-package)
		 (org-element-cache)
		 ((org-roam))
		 (comp)))
 '(warning-suppress-types '((use-package) (org-element-cache) ((org-roam)) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#E46876" :height 140 :italic t)) t)
 '(doom-modeline-time ((t (:inherit (mode-line-buffer-id bold) :box (:line-width (2 . 2) :color "dim gray" :style flat-button))))))
(put 'dired-find-alternate-file 'disabled nil)
