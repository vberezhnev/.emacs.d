(setq package-enable-at-startup nil) ;; Отключаем загрузку пакетов на старте
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

;; When using this directly, you will need to have use-package installed:
;; M-x package-install, select use-package. But if you start via
;; `standalone.el', this is being taken care of automatically.
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
				("melpa-stable" . "https://stable.melpa.org/packages/")
				("gnu" . "http://elpa.gnu.org/packages/")
				("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; install straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;(straight-use-package 'org)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq gui-p         (display-graphic-p)
      cli-p         (not gui-p)
      android-p     (getenv "ANDROID_ROOT")
      linux-p       (and (eq system-type 'gnu/linux) (not android-p))
      windows-p     (eq system-type 'windows-nt)
      workstation-p (member (system-name)
                            '("berezhnev")))

;; (if workstation-p
;;     (setq my/black (plist-get base16-chalk-theme-colors :base00)
;;           my/gray "thistle" ;;(plist-get base16-chalk-theme-colors :base01)
;;           my/lgray (plist-get base16-chalk-theme-colors :base03)
;;           ;; ... grayish colors from base02 to base06
;;           my/white (plist-get base16-chalk-theme-colors :base07)
;;           my/red (plist-get base16-chalk-theme-colors :base08)
;;           my/orange (plist-get base16-chalk-theme-colors :base09)
;;           my/yellow (plist-get base16-chalk-theme-colors :base0A)
;;           my/green (plist-get base16-chalk-theme-colors :base0B)
;;           my/lblue (plist-get base16-chalk-theme-colors :base0C)
;;           my/blue (plist-get base16-chalk-theme-colors :base0D)
;;           my/purple (plist-get base16-chalk-theme-colors :base0E)
;;           my/brown (plist-get base16-chalk-theme-colors :base0F)))

(if (not workstation-p)
    (setq my/black "black"
          my/gray  "light cyan"
          my/lgray "gray60"
          my/white "white"
          my/red "Firebrick"
          my/orange "orange red"
          my/yellow "lightyellow"
          my/green "ForestGreen"
          my/lblue "midnight blue"
          my/blue "blue"
          my/purple "Purple"
          my/brown "brown"))

(set-face-attribute
 'default nil
 :family "Source Code Pro"
 :height 130)

(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package system-packages
  :ensure t)

(defun frostyx/guix (&key install)
  (let ((system-packages-package-manager 'guix)
        (system-packages-use-sudo nil))
    (or (frostyx/rpm-query install)
 				(system-packages-install install))))

(defun frostyx/rpm-query (pack)
  (equal 0 (shell-command
            (concat "rpm -q " pack))))

(setq system-packages-package-manager 'guix)
(setq system-packages-use-sudo t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Templates2")))

(use-package ripgrep
  :ensure t)

(defun my-backward-delete-word (arg)
  "Delete word backwards without adding it to the kill-ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "M-DEL") 'my-backward-delete-word)
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word)

;; I find these light-weight and helpfulу

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; (use-package selectrum
;;   :ensure t
;;   :init
;;   (selectrum-mode)
;;   :custom
;;   (completion-styles '(flex substring partial-completion)))



;; Some common sense settings

(electric-pair-mode)
(global-auto-revert-mode t)

(define-coding-system-alias 'UTF-8 'utf-8)

(setq custom-file (concat user-emacs-directory "custom.el")
      mouse-yank-at-point t

      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file

      scroll-step 1
      scroll-margin 5
      scroll-conservatively 101

      initial-scratch-message ";; Do you even lisp, bro? ಠ_ಠ\n\n\n"
      backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
      kill-buffer-query-functions nil

      evil-shift-width 2
      tab-width 2
      mouse-autoselect-window t)

(setq-default shell-file-name "/bin/fish")

(setq flycheck-highlighting-mode 'lines)

(server-start)

(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)
(setq display-line-numbers-width 2)
(setq display-line-numbers-current-absolute t)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; (cond
;;  ((member "Monaco" (font-family-list))
;;   (set-face-attribute 'default nil :font "Monaco-14"))
;;  ((member "Inconsolata" (font-family-list))
;;   (set-face-attribute 'default nil :font "Inconsolata-14"))
;;  ((member "Consolas" (font-family-list))
;;   (set-face-attribute 'default nil :font "Consolas-14"))
;;  ((member "DejaVu Sans Mono" (font-family-list))
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono-14")))

;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t
;;         auto-package-update-interval 365
;;         auto-package-update-prompt-before-update nil
;;         auto-package-update-hide-results t)
;;   (auto-package-update-at-time "04:00"))

(use-package vterm
  :ensure t)

(use-package multi-vterm
	:bind (:map global-map ("C-x e" . multi-vterm))
	:ensure t)

;; (use-package bufler
;;   :bind (:map global-map ("C-x b" . bufler-switch-buffer))
;;   :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el"
;;                   :files (:defaults (:exclude "helm-bufler.el")))
;;   :config
;;   (use-package helm-bufler
;;     :quelpa (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
;;                          :files ("helm-bufler.el")))
;;   (setf bufler-groups
;;         (bufler-defgroups
;; 	  ;; Группа для ~/Templates2/Rust/t12stat и vterm буферов в этой директории
;; 	  (group
;;            (group-or "t12stat"
;;                      (dir "~/Templates2/Rust/t12stat")
;;                      (group-and "vterm in t12stat"
;; 				(mode-match "vterm" (rx bos "vterm-"))
;; 				(lambda (buffer)
;; 				  (with-current-buffer buffer
;; 				    (when (and (bound-and-true-p vterm-default-directory)
;;                                                (string-prefix-p "~/Templates2/Rust/t12stat"
;; 								(expand-file-name vterm-default-directory)))
;; 				      "t12stat"))))))
;; 	  ;; Остальные группы из стандартной конфигурации
;; 	  (group
;;            (auto-workspace))
;; 	  (group
;;            (group-or "*Help/Info*"
;;                      (mode-match "*Help*" (rx bos "help-"))
;;                      (mode-match "*Info*" (rx bos "info-"))))
;; 	  (group
;;            (group-and "*Special*"
;; 		      (lambda (buffer)
;; 			(unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
;; 				    (funcall (mode-match "Dired" (rx bos "dired")) buffer)
;; 				    (funcall (auto-file) buffer))
;; 			  "*Special*")))
;;            (group
;; 	    (name-match "**Special**"
;; 			(rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
;;            (group
;; 	    (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
;; 	    (auto-directory))
;;            (mode-match "*Helm*" (rx bos "helm-"))
;;            (auto-mode))
;; 	  (dir user-emacs-directory)
;; 	  (group
;;            (dir (if (bound-and-true-p org-directory)
;; 		    org-directory
;; 		  "~/org"))
;;            (group
;; 	    (auto-indirect)
;; 	    (auto-file))
;;            (group-not "*special*" (auto-file))
;;            (auto-mode))
;; 	  (group
;;            (auto-projectile))
;; 	  (group
;;            (auto-project))
;; 	  (auto-directory)
;; 	  (auto-mode))))


(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
	:config
	(display-battery-mode t)
	(display-time-mode t)
	(setq display-time-format "%H:%M") ;;  %d.%m.%Y
	(setq display-time-day-and-date t)
	(setq display-time-24hr-format t)
	(setq display-time-interval 60)
  (setq display-time-load-average nil)
	(setq doom-modeline-height 24
				all-the-icons-scale-factor 0.8
				doom-modeline-vcs-max-length 30
				doom-modeline-icon t
				doom-gruvbox-padded-modeline t
				doom-modeline-modal-modern-icon t
				doom-modeline-modal t
				doom-modeline-modal-icon t
				doom-modeline-buffer-file-name-style 'buffer-name
				doom-modeline-buffer-encoding nil
				doom-modeline-env-version nil
				doom-modeline-enable-word-count nil
				doom-modeline-position-line-format '("|%")
				doom-modeline-battery t
				doom-modeline-time t
				doom-modeline-mode-alist '())

	(doom-modeline-def-modeline 'my-custom-modeline
		;; Left
    '(bar buffer-info remote-host buffer-position word-count selection-info)
		;; right
    '(time battery major-mode misc-info process vcs))
  (doom-modeline-set-modeline 'my-custom-modeline t))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t)

(use-package olivetti
  :ensure t
  :straight (:host github :repo "rnkn/olivetti" :branch "master")
  ;; :demand t
  ;; :defer t
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (elfeed-search-mode . olivetti-mode)))
(setq olivetti-body-width 120)

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-display-icons-p t)
(setq dashboard-heading-icons
        '((recents   . "nf-cod-file")
          (bookmarks . "nf-cod-bookmark")         
          (projects  . "nf-cod-rocket")          
          (agenda    . "nf-cod-calendar")))
;; (setq dashboard-show-shortcuts nil)
(setq dashboard-init-info "")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package doom-dashboard
  :load-path "~/.emacs.d/lisp/packages/doom-dashboard"
  :after (dashboard nerd-icons)
  :demand t
  :bind
  (:map dashboard-mode-map
        ("<remap> <dashboard-previous-line>" . widget-backward)
        ("<remap> <dashboard-next-line>" . widget-forward)
        ("<remap> <previous-line>" . widget-backward)
        ("<remap> <next-line>" . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>" . widget-backward))
  :custom
  (dashboard-banner-logo-title " ")
  (dashboard-page-separator "\n")
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-items
     (lambda () (dashboard-insert-newline 2))
     dashboard-insert-init-info
     (lambda () (dashboard-insert-newline 2))
     doom-dashboard-insert-homepage-footer))
  (dashboard-item-generators
   '((recents . doom-dashboard-insert-recents-shortmenu)
     (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
     (projects . doom-dashboard-insert-project-shortmenu)
     (agenda . doom-dashboard-insert-org-agenda-shortmenu)))
  :config
  (setq dashboard-startup-banner
        (concat doom-dashboard-banner-directory "isometric3.txt"))
  (dashboard-setup-startup-hook))

(use-package hide-mode-line
  :ensure t)

(use-package zen-mode
  :ensure t)

(use-package bluetooth
  :ensure t
  :config
  (setq bluetooth-battery-warning-level 15))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq tramp-ssh-controlmaster-options "")

(load-file "~/.emacs.d/lisp/themes/twitch-dark-theme.el")

(use-package theme-changer
  :ensure t
  :demand t
  :straight (:host github :repo "hadronzoo/theme-changer" :branch "master")
  :config
  (setq calendar-location-name "Vladivostok, RU"
	calendar-latitude 43.11
	calendar-longitude 131.88)
  
  (change-theme 'leuven 'twitch-dark))

;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   (auto-dark-themes '((leuven-dark) (leuven)))
;;   :config
;;   (setq auto-dark-themes '((doom-gruvbox) (doom-one-light))
;; 	auto-dark-polling-interval-seconds 0)
  
;;   ;; auto-dark-allow-osascript nil
;;   ;; auto-dark-allow-powershell nil)
;;   (auto-dark-mode t))

(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "rust")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode"))

(load-file "~/.emacs.d/lisp/init-rust.el")
(load-file "~/.emacs.d/lisp/completion.el")
(load-file "~/.emacs.d/lisp/evil.el")
;; (load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/dired.el")
(load-file "~/.emacs.d/lisp/org/org.el")

 ;; (load-file "~/Downloads/Telegram Desktop/twitch-dark-theme.el")
