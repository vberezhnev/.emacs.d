;; (add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;; (setq package-archive-priorities '(("melpa"  . 3)
;;                                    ("gnu"    . 2)
;;                                    ("nongnu" . 1)))
;; (package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

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
 '(custom-safe-themes
	 '("2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" default))
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
