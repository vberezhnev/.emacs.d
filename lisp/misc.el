;;;;;;;;;;;;;;;;;;;;;;;;;;; THEMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(custom-safe-themes
	 '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5" default)))

(use-package doom-themes
  :if window-system
  :ensure t
  :defer nil
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (if (display-graphic-p)
      (progn
        (setq doom-themes-treemacs-theme "Default")
        (doom-themes-treemacs-config)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package gruvbox-theme
	:ensure t)

;; (use-package theme-changer
;;   :ensure t
;;   :demand t
;;   :straight (:host github :repo "hadronzoo/theme-changer" :branch "master")
;;   :config
;;   (setq calendar-location-name "Vladivostok, RU")
;;   (setq calendar-latitude 43.11)
;;   (setq calendar-longitude 131.88))

;; ;; (change-theme 'tsdh-light 'doom-xcode)

;; (use-package auto-dark
;;   :ensure t
;;   :config
;; 	(setq auto-dark-themes '((doom-gruvbox) (doom-one-light))
;; 				auto-dark-polling-interval-seconds 0)
;; 	(change-theme 'doom-one-light 'doom-gruvbox)

;; 	;; auto-dark-allow-osascript nil
;; 	;; auto-dark-allow-powershell nil)
;;   (auto-dark-mode t))

(load-theme 'doom-gruvbox)

;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :after all-the-icons
  :ensure t
  :defer nil
	:init
	(dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title "Build an epic shit"
        dashboard-startup-banner "~/.emacs.d/images/Emacs-logo.svg"
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        ;; dashboard-startupify-list     nil
        dashboard-set-heading-icons t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-set-file-icons    t
				dashboard-items '((recents  . 10)
                          ;; (agenda   . 3)
                          (projects . 6)
													(bookmarks . 3))))

(use-package fontawesome
  :ensure t
	;; :config
	;; (set-fontset-font t 'unicode "FontAwesome" nil 'prepend)
	)

(use-package all-the-icons
  :defer t
  :ensure t)

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

;; (use-package spacious-padding
;; 	:ensure t
;; 	:init
;; 	(spacious-padding-mode 1)
;; 	:config
;; 	(setq spacious-padding-widths
;; 				'( :internal-border-width 0
;; 					 :header-line-width 2
;; 					 :mode-line-width 6
;; 					 :tab-width 2
;; 					 :right-divider-width 300
;; 					 :scroll-bar-width 8
;; 					 :fringe-width 0)))

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

(use-package nerd-icons
  :ensure)

(use-package hide-mode-line
  :ensure t
  :config
  (evil-leader/set-key
    "h" 'global-hide-mode-line-mode))

(use-package olivetti
  :ensure t
  :demand t
  :defer t
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

(use-package helm
  :ensure t
	:bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
         ;; ("C-x b" . helm-mini)
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-fuzzy-match t)
	(setq helm-move-to-line-cycle-in-source nil)

  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)

  (define-key helm-map [escape] 'helm-keyboard-quit))

(use-package helm-posframe
  :ensure t
  :config
  ;; (setq helm-posframe-width 200)
  ;; (setq helm-posframe-height 600)
	(setq helm-posframe-parameters '((left-fringe . 10) (right-fringe . 10)))
  (setq helm-posframe-poshandler
        'posframe-poshandler-frame-center)
  (helm-posframe-enable))

(use-package which-key-posframe
	:ensure t
	:config
  (which-key-posframe-mode))

;; (use-package company-posframe
;; 	:ensure t
;;   :config
;;   (setq company-posframe-show-metadata nil)
;;   (setq company-posframe-show-indicator nil)
;;   (setq company-posframe-quickhelp-delay nil)
;;   (company-posframe-mode +1))

(use-package flycheck-posframe
	:ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-left-corner)
  (flycheck-posframe-warning-prefix "\u26a0 ")
  (flycheck-posframe-error-prefix "\u26a0 "))

;; (use-package mini-frame
;; 	:ensure t
;; 	:config
;; 	(mini-frame-mode t))

;; (custom-set-variables
;;  '(mini-frame-show-parameters
;;    '((top . 10)
;;      (width . 0.7)
;;      (left . 0.5))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package rainbow-mode
  :ensure t
  :config
  nil)


(use-package treemacs
  :ensure t
  :defer t
	:demand t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
