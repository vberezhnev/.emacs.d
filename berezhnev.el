;; (server-mode)
;;(setq max-lisp-eval-depth 50000)

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))

(setq gui-p         (display-graphic-p)
      cli-p         (not gui-p)
      android-p     (getenv "ANDROID_ROOT")
      linux-p       (and (eq system-type 'gnu/linux) (not android-p))
      windows-p     (eq system-type 'windows-nt)
      workstation-p (member (system-name)
                            '("berezhnev")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;; https://www.emacswiki.org/emacs/AutoPairs
;; TODO try https://github.com/Fuco1/smartparens instead
(electric-pair-mode)


;; Middle-click paste where cursor is, don't care about mouse position
;; https://superuser.com/questions/330849/can-i-tell-emacs-to-paste-middle-mouse-button-on-the-cursor-position
(setq mouse-yank-at-point t)


;; Automatically reload changed files
;; https://stackoverflow.com/q/1480572/3285282
(global-auto-revert-mode t)


;; keyboard scroll one line at a time
;; scroll when cursor is five lines from the edge
;; and don't ever recenter the cursor when scrolling
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 101)


;; Don't throw "Invalid coding system" errors on me when writting a file
(define-coding-system-alias 'UTF-8 'utf-8)


;; Don't mess CWD with #foo.py# and foo.py~ files
;; https://emacs.stackexchange.com/a/34
;; (setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
;; (setq backup-directory-alist '("~/MyEmacsBackups"))
;; (setq backup-directory-alist `(("." . "~/MyEmacsBackups")))


;; https://stackoverflow.com/a/18330742/3285282
(setq backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      display-line-numbers 'relative
      )

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (setq display-line-numbers 'relative)
  (setq display-line-numbers-width 3))

(save-place-mode 1)

(setq initial-scratch-message ";; Do you even lisp, bro? ಠ_ಠ\n\n\n")

;; (setq confirm-kill-emacs 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(toggle-frame-fullscreen)

(xterm-mouse-mode t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package no-littering
  :ensure t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 365
         auto-package-update-prompt-before-update nil
         auto-package-update-hide-results t)
   (auto-package-update-at-time "04:00"))

(setq kill-buffer-query-functions nil)

(add-hook 'auto-package-update-after-hook
          (lambda ()
            (load-library "yasnippet-snippets.el")))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;; (setq quelpa-upgrade-p t)

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

(use-package password-store
  :ensure t)

(use-package ob-async
	:ensure t)

(use-package general
  :ensure t)

(general-evil-setup)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package all-the-icons-ibuffer
	:ensure t
	:config
																				; Predicate whether the icons are able to be displayed."
	(setq all-the-icons-ibuffer-display-predicate #'display-graphic-p)

	;; Whether display the icons.
	(setq all-the-icons-ibuffer-icon t)

	;; Whether display the colorful icons.
	;; It respects `all-the-icons-color-icons'.
	(setq all-the-icons-ibuffer-color-icon t)

	;; The default icon size in ibuffer.
	(setq all-the-icons-ibuffer-icon-size 1.0)

	;; The default vertical adjustment of the icon in ibuffer.
	(setq all-the-icons-ibuffer-icon-v-adjust 0.0)

	;; Use human readable file size in ibuffer.
	(setq  all-the-icons-ibuffer-human-readable-size t)

	;; A list of ways to display buffer lines with `all-the-icons'.
	;; See `ibuffer-formats' for details.
	all-the-icons-ibuffer-formats

	;; Slow Rendering
	;; If you experience a slow down in performance when rendering multiple icons simultaneously,
	;; you can try setting the following variable
	(setq inhibit-compacting-font-caches t)

	(add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode))

(use-package ultra-scroll
  :quelpa (ultra-scroll
          :fetcher github
          :repo "jdtsmith/ultra-scroll"
          :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

;; (pixel-scroll-precision-mode)

(general-define-key
 :states '(emacs normal insert motion)
 "C-y" 'mouse-yank-primary)

(defun get-word-boundary ()
  "Return the boundary of the current word.
 The return value is of the form: (cons pos1 pos2)."
  (let ((chars  "-A-Za-z0-9_.@/"))
    (save-excursion
      (let (p1 p2)
        (progn
          (skip-chars-backward chars)
          (setq p1 (point))
          (skip-chars-forward chars)
          (setq p2 (point)))
        (cons p1 p2)))))

(defun select-word ()
  "Mark the url under cursor."
  (interactive)
  (let (bds)
    (setq bds (get-word-boundary))
    (set-mark (car bds))
    (goto-char (cdr bds))))

(global-set-key [double-mouse-1] 'select-word)

(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  ;; (setq evil-vsplit-window-right t)
  ;; (setq evil-split-window-below t)
  ;;(setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)

  (setq evil-ex-set-initial-state 'normal)

  :config ;; tweak evil after loading it
  (evil-mode))

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) (kbd "<up>") nil)
    (define-key (eval map) (kbd "<down>") nil)
    (define-key (eval map) (kbd "<left>") nil)
    (define-key (eval map) (kbd "<right>") nil)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package evil-terminal-cursor-changer
	:ensure t
	:config
	(unless (display-graphic-p)
          (require 'evil-terminal-cursor-changer)
          (evil-terminal-cursor-changer-activate))
	 (setq evil-motion-state-cursor 'box)  ; █
     (setq evil-visual-state-cursor 'box)  ; █
     (setq evil-normal-state-cursor 'box)  ; █
     (setq evil-insert-state-cursor 'bar)  ; ⎸
     (setq evil-emacs-state-cursor  'hbar)) ; _

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

;; ;; @FIXME missing config
;; (use-package evil-indent-textobject
;;   :ensure t)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-set-option
  :ensure t
  :quelpa (evil-set-option
           :fetcher github
           :repo "FrostyX/evil-set-option"
           :branch "main")
  :config
  (evil-set-option-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'org-super-agenda-mode 'emacs)
  (evil-set-initial-state 'calibredb-mode 'normal)
  (evil-set-initial-state 'enlight-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

;; use evil mode in the buffer created from calling `M-x list-packages'.
;; https://blog.aaronbieber.com/2016/01/23/living-in-evil.html#adding-hjkl-bindings-
(evil-add-hjkl-bindings occur-mode-map 'emacs
  (kbd "/")       'evil-ex-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "C-w C-w") 'other-window)

(use-package magit
	:ensure t)

(evil-leader/set-key
  "ga" 'magit-stage-file
  "gc" 'magit-commit  ;; Maybe magit-commit-create
  "gp" 'magit-push-current) ;; @TODO still asks for something, use more specific function

(evil-leader/set-key
  "w" 'evil-window-vsplit)
  ;;"def" 'evil-jump-to-tag)

(general-define-key
 :states '(normal visual insert emacs motion)
 "C-x l" 'evil-switch-to-windows-last-buffer
 "C-x SPC" 'hydra-main/body
 "C-x ;"  'my/smart-buffers-list
 "C-x , ;" 'consult-buffer)

(use-package base16-theme
  :ensure t)

(use-package seoul256-theme
  :ensure t
  :disabled t)

(use-package gruvbox-theme
  :ensure t
  :disabled t)

(use-package doom-themes
  :if window-system
  :ensure t
  :defer nil
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (if (display-graphic-p)
      (progn
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(if workstation-p
   (load-theme 'base16-chalk t))

(use-package theme-changer
  :ensure nil
  :demand nil
  :straight (:host github :repo "hadronzoo/theme-changer" :branch "master")
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88))

;;(change-theme 'tsdh-light 'doom-xcode)
;;(change-theme 'doom-one-light 'doom-outrun-electric)
(load-theme 'doom-one-light)

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-dark-theme 'doom-xcode)
  (setq auto-dark-light-theme 'doom-one-light) ;; tsdh-light
  (setq auto-dark-polling-interval-seconds 0)
  (setq auto-dark-allow-osascript nil)
  (setq auto-dark-allow-powershell nil)
  (auto-dark-mode t))

;; (set-frame-parameter (selected-frame) 'alpha-background 70)
;; (add-to-list 'default-frame-alist '(alpha-background . 70))

(set-frame-parameter (selected-frame) 'alpha '(98 . 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))

(if workstation-p
    (setq my/black (plist-get base16-chalk-theme-colors :base00)
          my/gray "thistle" ;;(plist-get base16-chalk-theme-colors :base01)
          my/lgray (plist-get base16-chalk-theme-colors :base03)
          ;; ... grayish colors from base02 to base06
          my/white (plist-get base16-chalk-theme-colors :base07)
          my/red (plist-get base16-chalk-theme-colors :base08)
          my/orange (plist-get base16-chalk-theme-colors :base09)
          my/yellow (plist-get base16-chalk-theme-colors :base0A)
          my/green (plist-get base16-chalk-theme-colors :base0B)
          my/lblue (plist-get base16-chalk-theme-colors :base0C)
          my/blue (plist-get base16-chalk-theme-colors :base0D)
          my/purple (plist-get base16-chalk-theme-colors :base0E)
          my/brown (plist-get base16-chalk-theme-colors :base0F)))

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

(use-package ibuffer-vc
	:ensure t)

(defun my/center (width)
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package gitignore-templates
  :ensure t)

(setq gitignore-templates-api 'github)

(use-package pdf-tools
  :ensure t
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  (use-package saveplace-pdf-view  )
  (save-place-mode 1)
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("\\" . hydra-pdftools/body)
              ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
              ("g"  . pdf-view-first-page)
              ("G"  . pdf-view-last-page)
              ("l"  . image-forward-hscroll)
              ("h"  . image-backward-hscroll)
              ("j"  . pdf-view-next-page)
              ("k"  . pdf-view-previous-page)
              ("e"  . pdf-view-goto-page)
              ("u"  . pdf-view-revert-buffer)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("am" . pdf-annot-add-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("y"  . pdf-view-kill-ring-save)
              ("i"  . pdf-misc-display-metadata)
              ("s"  . pdf-occur)
              ("b"  . pdf-view-set-slice-from-bounding-box)
              ("r"  . pdf-view-reset-slice)))

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(general-nmap "C-n" 'dired-jump)

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t)

(setq dired-open-extensions
      '(("gif" . "eog")
        ("jpg" . "eog")
        ("png" . "eog")
        ("mkv" . "vlc --one-instance")
        ("mp4" . "vlc --one-instance")
        ("ogv" . "vlc --one-instance")
        ("avi" . "vlc --one-instance")
        ("odt"  . "libreoffice")
        ("docx" . "libreoffice")
        ("docx" . "libreoffice")))

(use-package helm-dired-open
  :ensure t
  :quelpa (helm-dired-open
           :fetcher github
           :repo "FrostyX/helm-dired-open"))

(defun my/dired-vlc-enqueue ()
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit))))
	(dired-open--start-process file "vlc --one-instance --playlist-enqueue")))

(use-package dired-rainbow
  :ensure t)

(dired-rainbow-define img nil ("gif" "jpg" "png"))
(dired-rainbow-define video nil ("mp4" "mkv" "ogv" "avi"))
(dired-rainbow-define office nil ("odt" "doc" "docx"))
(dired-rainbow-define pdf nil ("pdf"))

(set-face-attribute 'dired-rainbow-img-face nil :foreground my/green)
(set-face-attribute 'dired-rainbow-office-face nil :foreground my/yellow)
(set-face-attribute 'dired-rainbow-video-face nil :foreground my/orange)
(set-face-attribute 'dired-rainbow-pdf-face nil :foreground my/purple)

(use-package dired
  :hook (dired-mode . hl-line-mode))

(general-unbind 'normal dired-mode-map
  "SPC"
  "S-SPC")

(use-package archive-rpm
  :ensure t)

(use-package udiskie
  :if workstation-p
  :ensure t
  :disabled t
  :quelpa (udiskie
           :fetcher git
           :url "https://gitlab.com/tuedachu/udiskie.el.git"))

(use-package git-gutter
	:ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
	:ensure t
	:config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package ox-reveal
	:ensure t)

(use-package telega
  :ensure t
  :commands (telega)
  :config
  (setq telega-use-docker t))

(defun elfeed-search-format-date (date)
  "")

(setq elfeed-search-trailing-width 15)

(setq elfeed-search-print-entry-function #'frostyx/elfeed-search-print-entry)

(defun frostyx/elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    ;; The whole function is copy-pasted `elfeed-search-print-entry--default',
    ;; just the following lines are changed
    (when feed-title
      (insert (string-pad
               (propertize feed-title 'face 'elfeed-search-feed-face) 15)))
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when tags
      (insert "(" tags-str ")"))))

(use-package ednc
  :quelpa (ednc
           :fetcher github
           :repo "FrostyX/ednc"
           :branch "ednc-faces")
  :config
  (ednc-mode 1))

(set-face-attribute 'ednc-app-name nil :foreground my/blue)
(set-face-attribute 'ednc-title nil :foreground my/green)
(set-face-attribute 'ednc-body nil :foreground my/yellow)

(general-nmap
  :keymaps 'ednc-view-mode-map
  "TAB" 'ednc-toggle-expanded-view)

(defun frostyx/autoscroll ()
  (set (make-local-variable 'window-point-insertion-type) t))

(add-hook 'ednc-view-mode-hook 'frostyx/autoscroll)

;; Needed for `:after char-fold' to work
;; (use-package char-fold
;; 	:ensure t
;;   :custom
;;   (char-fold-symmetric t)
;;   (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  ;; :ensure t ; install `reverse-im' using package.el
  :quelpa (reverse-im
           :fetcher github
           :repo "emacsmirror/reverse-im")
  :demand t
  ;;:after char-fold ; but only after `char-fold' is loaded
  :custom
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t)) ; turn the mode on

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Templates/")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(defun frostyx/set-default-font (size)
  (set-face-attribute
   'default nil
   :family "Iosevka"
   ;;:foundry "ADBO"
   :height 150 ;;size
   :weight 'regular ;;'normal
   :width 'normal
   :slant 'normal
   ;;:foreground (plist-get base16-chalk-theme-colors :base05)
))

(frostyx/set-default-font 90)

(set-face-attribute 'lazy-highlight nil :background my/orange
                                        :foreground my/gray)

(use-package dashboard
  :after all-the-icons
  :ensure t
  :defer nil
  :config
  (setq dashboard-banner-logo-title "Welcome back, Darling!"
        dashboard-startup-banner "~/.emacs.d/images/Emacs-logo.svg"
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        dashboard-set-navigator     nil
        dashboard-set-heading-icons t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-set-file-icons    t)
  (setq dashboard-items '((recents  . 12)
                          ;; (agenda   . 6)
                          (projects . 6))))

(dashboard-setup-startup-hook)

(use-package fontawesome
  ;; :disabled nil
  :ensure t)

(set-fontset-font t 'unicode "FontAwesome" nil 'prepend)

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

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

;; Make it smaller
(setq doom-modeline-height 22)

;; By default the icons are larger than the rest of the text, we don't want that
(setq all-the-icons-scale-factor 1.0)

;; Don't show icons, I have some bug rendering them incorrectly
(setq doom-modeline-icon nil)

;; Display only buffer names, not full paths
(setq doom-modeline-buffer-file-name-style 'buffer-name)

;; Don't show buffer encoding
(setq doom-modeline-buffer-encoding nil)

;; Don't show any mess next to major-mode
(setq doom-modeline-env-version nil)

(use-package nerd-icons
  :ensure)

(setq doom-modeline-mode-alist '())

(use-package hide-mode-line
  :ensure t
  :config
  (evil-leader/set-key
    "h" 'global-hide-mode-line-mode))

(use-package olivetti
  :ensure t
  :defer t
  :hook ((text-mode         . olivetti-mode)
         ;; (org-agenda-mode   . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode))
  :config
  (setq olivetti-body-width 150))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-fuzzy-match t)


  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  ; (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  ; http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)

  (define-key helm-map [escape] 'helm-keyboard-quit)

  ; (define-key helm-map (kbd "C-S-h") 'describe-key)
  ; (define-key helm-map (kbd "C-l") (kbd "RET"))
  ; (dolist (keymap (list helm-find-files-map helm-read-file-map))
        ; (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
        ; (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
        ; (define-key keymap (kbd "C-S-h") 'describe-key))


)

(setq helm-move-to-line-cycle-in-source nil)

(use-package helm-posframe
  :ensure t
  :disabled
  :config
  (helm-posframe-enable)
  (setq helm-posframe-poshandler
        #'posframe-poshandler-frame-center)
  (setq helm-posframe-width 200)
  (setq helm-posframe-height 600)
  (setq helm-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

(defvar spacemacs-helm-display-help-buffer-regexp '("\\*.*Helm.*Help.*\\*"))
(defvar spacemacs-helm-display-buffer-regexp `("\\*.*helm.*\\*"
                                               (display-buffer-in-side-window)
                                               (inhibit-same-window . nil)
                                               (side . bottom)
                                               (window-width . 0.6)
                                               (window-height . 0.6)))

(defun display-helm-at-bottom (buffer &optional _resume)
  (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                    spacemacs-helm-display-buffer-regexp)))
    (display-buffer buffer)))
(setq helm-display-function 'display-helm-at-bottom)

(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 40)

(defun my/smart-buffers-list ()
  (interactive)
  (cond ((derived-mode-p 'lui-mode) (my/circe-switch-to-buffer))

        ((derived-mode-p 'vterm-mode)
         (if (projectile-project-root)
             (frostyx/projectile-switch-to-vterm-buffer)
           (frostyx/switch-to-vterm-buffer)))

        ((derived-mode-p 'ement-room-list-mode
                         'ement-room-mode
                         'ement-directory-mode)
         (helm-ement-buffers))

        ((projectile-project-root) (consult-project-buffer))
        (t (consult-buffer))))

(general-nmap ";"  'my/smart-buffers-list)
(general-nmap ",;" 'consult-buffer)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package rainbow-mode
  ;; There is a bug visualizing even #def in .Xdefaults
  :ensure t
  :config
  nil)

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color my/gray))

(defun my/colorcolumn (column)
  (turn-on-fci-mode)
  (set-fill-column column))

(set-face-attribute 'fill-column-indicator nil :foreground my/lgray)

(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode)
  (dtrt-indent-adapt))

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq tab-width 2)

(defun hook-tab-width ()
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq python-indent-offset 2))
(add-hook 'prog-mode-hook #'hook-tab-width)

(setq evil-shift-width 2)

(setq python-indent-offset 2)

(use-package company
    :ensure t
    :hook
    (company-mode . frostyx/company-mode-hook)
    :config
;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode))

(defun frostyx/company-mode-hook ()
  (setq-local evil-complete-next-func 'frostyx/company-complete))

(defun frostyx/company-complete (&optional arg)
  (interactive)
  (company-complete-common-or-cycle))

(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)

(setq company-icon-margin 3)

(add-hook 'after-init-hook 'company-tng-mode)

(setq company-tng-auto-configure nil)
(with-eval-after-load 'company-tng
  (setq company-active-map company-tng-map))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode)

  :config
  (setq company-box-icon-right-margin 1))

(setq company-frontends '(company-tng-frontend company-box-frontend))

(use-package company-org-block
  :ensure t
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package diff-hl
  :ensure t)

(use-package flycheck
  :ensure t)

(setq flycheck-highlighting-mode 'lines)

(setq-default flycheck-flake8-maximum-line-length 120)

(flycheck-add-next-checker 'python-flake8 'python-pylint)

(setq-default flycheck-pylint-use-symbolic-id nil)

(defun pylint-id-to-symbolic (msg-id)
  (let* ((cmd (list "pylint" "--help-msg" msg-id))
         (msg (shell-command-to-string (combine-and-quote-strings cmd)))
         (start (+ (string-match ":" msg) 1))
         (end (string-match ":" msg start))
         (name (substring msg start end)))
    (car (split-string name " "))))

(defun pylint-disable-warning (msg-id)
  (end-of-line)
  (insert "  " (format "# pylint: disable=%s" msg-id)))

(defun pylint-current-error ()
  (first (flycheck-overlay-errors-at (point))))

(defun pylint-disable-current-warning ()
  (interactive)
  (pylint-disable-warning
    (pylint-id-to-symbolic
      (flycheck-error-id
        (pylint-current-error)))))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(setq yas-snippet-dirs
      (list (expand-file-name "snippets/" user-emacs-directory)
            yasnippet-snippets-dir))
(yas-reload-all)

(use-package scratch
  :ensure t)

(defun frostyx/scratch-for-major-mode ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; Emulate C-u
    (call-interactively 'scratch)))

;; (add-hook 'after-init-hook 'org-agenda-list)

(use-package org
  :straight (:type built-in)
  :bind (("C-c C-x C-j" . org-clock-goto))
  :ensure nil
  ;; :defer t
  ;; :after org
  ;; :demand t
  ;; :delight org-mode "✎"
  ;; :hook ((org-mode . prettify-symbols-mode)
  ;;        (org-mode . visual-line-mode)
  ;;        (org-mode . variable-pitch-mode))
  :bind (("C-c l"               . org-store-link)
         ;; ("C-c c"               . org-capture)
         ("C-c f"               . org-footnote-new)))

(define-key global-map (kbd "C-c u") #'calendar)

(setq-default org-reverse-datetree-level-formats
              '("Week №%W {%B-%Y}"))

(setq org-capture-templates
      '(("c" "New task" entry (file "~/Org/agenda/GTD/Inbox.org")
         "* TODO %?")

        ("p" "PROGRAMMING (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org"
                                                              (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("💻 Programming 👾"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t" :clock-in t)

        ("m" "Meeting" entry (file+function "~/Org/Meetings.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("Meetings"))))
         "* Meeting for %U\nSCHEDULED: %U"
         :clock-in t
         :time-prompt t)

        ("e" "EXAMS (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("📖 Exams 📖"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t")))

(setq org-datetree-add-timestamp t)

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(setq
 org-ellipsis " ⤵" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
 org-roam-v2-ack t                 ; anonying startup message
 ;; org-log-done 'time                ; I need to know when a task is done
 org-hide-leading-stars t
 org-log-into-drawer t
 org-startup-folded t
 ;; org-odd-levels-only t
 org-pretty-entities t
 org-startup-indented t
 org-adapt-indentation t
 org-hide-macro-markers t
 org-hide-block-startup nil
 ;; org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-cycle-separator-lines 2
 org-startup-with-inline-images t
 org-display-remote-inline-images t
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 2
 ;; org-fontify-quote-and-verse-blocks t
 org-export-with-smart-quotes t

 org-checkbox-hierarchical-statistics nil
 org-read-date-prefer-future 'time
 org-agenda-todo-ignore-scheduled 'future
 org-agenda-tags-todo-honor-ignore-options t
 org-agenda-todo-ignore-with-date t
 org-image-actual-width '(300)
 org-log-done (quote time)
 ;; Don't log the time a task was rescheduled or redeadlined.
 org-log-redeadline t ; changed
 org-log-reschedule t)
 org-todo-keyword-faces
 ;; '(
 ;;   ("TODO" :background "indian red" :foreground "white" :weight bold)
 ;;   ("NEXT" :background "sky blue" :foreground "black" :weight bold)
 ;;   ("WAIT" :background "olive drab" :foreground "black" :weight bold)
 ;;   ("DONE" :background "pale green" :foreground "black" :weight bold)
 ;;   ("CNCL" :background "dark red" :foreground "white" :weight bold))
;; org-todo-keywords
;; '((sequence "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)")) ; changed

;; ("DOING" :background "tomato" :foreground "white" :weight bold)
;; ("STOPPED" :background "firebrick2" :foreground "white" :weight bold)
;; ("REVIEW" :background "cyan" :foreground "black" :weight bold)
;; ("ARCHIVED" :background "light slate blue" :foreground "white" :weight bold)
;; (setq )

;; (setq )

;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(with-eval-after-load 'org
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-modules 'org-tempo t))

(evil-leader/set-key
        "z" '(org-agenda nil "z"))

(global-set-key (kbd "C-c C-x o") 'org-clock-out)
(global-set-key (kbd "C-c C-x j") 'org-clock-go-to)

;; (defun my-org-clock-in-with-sound ()
;;   "Clock in to an org item, play a sound notification, and truncate the task name if it's too long."
;;   (interactive)
;;   (org-clock-in)  ;; Start the clock
;;   (play-sound-file "~/path/to/sound.wav")  ;; Play the sound

;;   ;; Check and truncate the clocked-in task name if needed
;;   (when (org-clock-is-active)
;;     (let* ((task-name (substring-no-properties (org-clock-get-clock-string)))
;;            (truncated-task-name (if (> (length task-name) 25)
;;                                     (concat (substring task-name 0 22) "...")
;;                                   task-name)))
;;       (message "Clocked in: %s" truncated-task-name))))

;; (global-set-key (kbd "C-c k") 'my-org-clock-in-with-sound)

(use-package org-agenda
  :ensure nil
  :straight (:type built-in)
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config
  ;; Function to be run when org-agenda is opened

  ;; Adds hook to org agenda mode, making follow mode active in org agenda
  ;;(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)
  ;;(add-hook 'org-agenda-mode-hook 'toggle-truncate-lines)

  (setq org-agenda-start-on-weekday 0
        org-agenda-skip-scheduled-if-done t ; changed
        org-agenda-skip-deadline-if-done t ; changed
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t ; changed
        org-agenda-start-with-log-mode nil
    		org-agenda-deadline-faces
        '((1.0001 . org-warning)              ; due yesterday or before
          (0.0    . org-upcoming-deadline))   ; due today or later
    		org-icalendar-combined-name "Hugo Org"
    		org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
    		org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
    		org-icalendar-timezone "Asia/Vladivostok"
    		org-icalendar-store-UID t
    		org-icalendar-alarm-time 30
    		calendar-date-style 'european
    		calendar-week-start-day 0
        calendar-mark-holidays-flag t
        calendar-mark-diary-entries-flag nil
  			;; (setq-default org-icalendar-include-todo t)
    		org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((today require-timed remove-match)
                               (500 800 1000 1200 1400 1600 1800 2000)
                               ":  " "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format
  			'((agenda . "%-10c | %?-12t% s")
  				(todo . "%-10s")
  				(tags . "%t %-10c | %s")
  				(search . "%c %t %s"))
        org-agenda-clockreport-parameter-plist
        (quote (:maxlevel 5 :compact t :wstart 0 :link nil :formula % :tags nil :properties ("CATEGORY" "EFFORT") :narrow 80 :fileskip0 t))
        org-agenda-scheduled-leaders '("[S]:" "[S] x%3dd.:")
        org-agenda-deadline-leaders '("[D]:" "[D] +%3dd.:" "[D] -%3dd.:")
    		org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date)))
    		org-default-notes-file "~/Org/agenda/Notes.org"
    		org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")) ;; "~/Org/agenda/Calendar.org"

  (setq mixed-pitch-fixed-pitch-faces
				(quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))

	;; Hide duplicates of the same todo item
	;; If it has more than one of timestamp, scheduled,
	;; or deadline information
  (setq org-agenda-skip-timestamp-if-done t
				org-agenda-skip-deadline-if-done t
				org-agenda-skip-scheduled-if-done t
				org-agenda-skip-scheduled-if-deadline-is-shown t
				org-agenda-skip-timestamp-if-deadline-is-shown t)

  ;; (setq org-agenda-clockreport-parameter-plist
  ;;       (quote (:link t :maxlevel 5 :fileskip t :compact t :narrow 80)))

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.3)
    (set-face-attribute 'org-agenda-date-today nil :height 1.3 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.3))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-custom-commands
        '(("c" "Getting Things Done (GTD)"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-skip-deadline-if-done nil)
                        (org-agenda-clockreport-mode t)
                        (org-agenda-remove-tags t)
                        (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
                        (org-time-budgets-in-agenda-maybe)
                        (org-agenda-include-deadlines t)

                        (org-agenda-files '("~/Org/agenda/PlanAhead.org" "~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Schedule"
    															:time-grid t)
                           (:name "Today"
    															:scheduled today
    															:face (:background "medium sea green" :foreground "white")
                                  :face 'warning)
                           (:name "Future deadline"
    															:deadline future
    															:face (:background "deep sky blue"))
                           (:name "Deadline today"
    															:deadline today
    															:face (:background "black" :foreground "white"))
                           (:name "Passed deadline"
    															:deadline past
                                  :scheduled past
    															:face (:background "salmon"))))))

            ;; (alltodo "" ((org-agenda-overriding-header "")
            ;;              (org-agenda-prefix-format "  %?-12t% s")
            ;;              (org-agenda-entry-text-mode t)
            ;;              (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")) ;; "~/Org/agenda/GTD/Projects.org"
            ;;              (org-super-agenda-groups
            ;;               '((:name "Tasks ready to actions"
    				;; 											 :children t
    				;; 											 :todo "NEXT")))))

            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Everytime"
    															:tag ("everytime"))
    											 (:name "Morning"
    													  	:tag ("morning"))
    											 (:name "Day"
    													  	:tag ("day"))
    											 (:name "Evening"
    													  	:tag ("evening"))
    											 ;; (:name "Challenges"
    											 ;;  			:tag "challenge")
    											 (:discard (:anything))
    											 (:discard (:not (:tag "habit")))))))))
          ("p" "Private counter"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "===== Other ====="
    												      :tag "other"
                                  :face (:background "red" :foreground "white" :weight "bold"))
    											 (:discard (:anything))
    											 (:discard (:not (:tag "habit")))))))))

          ("d" "Day results"
					 ((agenda ""
										((org-agenda-span 'day)
										 (org-agenda-overriding-header "\n === TIME REPORT ===")
										 (org-agenda-skip-scheduled-if-done nil)
										 (org-log-done 'time)
										 (org-log-into-drawer t)
										 (org-agenda-skip-deadline-if-done nil)
										 (org-agenda-clockreport-mode t)
										 (org-agenda-remove-tags t)
										 (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
										 (org-time-budgets-in-agenda-maybe)
										 (org-agenda-include-deadlines t)
										 (org-agenda-clockreport-parameter-plist
											'(:scope ("~/Org/agenda/GTD/org-gtd-tasks.org"
																"~/Org/agenda/GTD/gtd_archive_2025"
																"~/Org/agenda/GTD/gtd_archive_2024"
																"~/Org/agenda/GTD/org-gtd-tasks.org_archive")
															 :maxlevel 5
															 :emphasize t
															 :block day
															 :compact t
															 :wstart 0
															 :link nil
															 :formula %
															 :tags nil
															 :properties ("CATEGORY" "EFFORT")
															 :fileskip0 t))
										 (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
																				 "~/Org/agenda/GTD/gtd_archive_2025"
																				 "~/Org/agenda/GTD/gtd_archive_2024"
																				 "~/Org/agenda/GTD/org-gtd-tasks.org_archive"))
										 (org-super-agenda-groups '((:discard (:anything))))))

						(tags "CLOSED>=\"<today>\""
									((org-agenda-overriding-header "\n === COMPLETED TASKS ===")))

						(tags "+STYLE=\"habit\"+CLOSED>=\"<today>\""
									((org-agenda-overriding-header "\n === COMPLETED HABITS ===")))))
					))

  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(use-package org-ref
	:quelpa (org-ref
       :fetcher github
       :repo "jkitchin/org-ref"
       :branch "master")
	;;:ensure t
  :config
	;;(require 'org-ref)
	;;(require 'org-ref-helm)
	(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

	(setq bibtex-completion-bibliography '("~/Org/Bibliography/Bibliography.bib")
				bibtex-completion-library-path '("~/Org/Bibliography/files/")
				bibtex-completion-notes-path "~/Org/Bibliography/notes/"
				bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

				bibtex-completion-additional-search-fields '(keywords)
				bibtex-completion-display-formats
				'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
					(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
					(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
					(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
				bibtex-completion-pdf-open-function
				(lambda (fpath)
					(call-process "open" nil 0 nil fpath)))
	(setq bibtex-autokey-year-length 4
				bibtex-autokey-name-year-separator "-"
				bibtex-autokey-year-title-separator "-"
				bibtex-autokey-titleword-separator "-"
				bibtex-autokey-titlewords 2
				bibtex-autokey-titlewords-stretch 1
				bibtex-autokey-titleword-length 5))

(use-package org-roam-bibtex
  :after org-roam
	:quelpa (org-roam-bibtex
       :fetcher github
       :repo "org-roam/org-roam-bibtex"
       :branch "main"))
  ;;:config
  ;;(require 'org-ref) ; optional: if using Org-ref v2 or v3 citation links

(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))

(org-link-set-parameters "zotero" :follow 'my-org-zotero-open)

(use-package citar
	:quelpa (citar
       :fetcher github
       :repo "emacs-citar/citar"
       :branch "main")
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Org/Bibliography/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :config
  (defvar citar-indicator-notes-icons
		(citar-indicator-create
		 :symbol (all-the-icons-material
							"speaker_notes"
							:face 'all-the-icons-blue
							:v-adjust -0.3)
		 :function #'citar-has-notes
		 :padding "  "
		 :tag "has:notes")))

(use-package tsc
  :ensure t)

(use-package ob-typescript
	:ensure t)
(use-package ob-rust
	:ensure t)
(use-package ob-solidity
	:ensure t)
(use-package ob-sql-mode
	:ensure t)
(use-package ob-restclient
  :ensure t)
(use-package gnuplot
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js         . t)
   (solidity   . t)
   (typescript . t)
   (shell      . t)
   (python     . t)
   (rust       . t)
   (C          . t)
   (sql        . t)
   (latex      . t)
   (restclient . t)
   (gnuplot    . t)))

(setq org-tag-alist
        '(
  				("@article" . ?a)
          ("@personal" . ?P)
          ("@coding" . ?p)
          ("@mathematics" . ?m)
  				("@school" . ?s)
          ("@english" . ?e)
  				("@work" . ?w)
          ("@zettelkasten" . ?z)
  				("@idea" . ?i)))

  (use-package org-modern
    :hook (org-mode . org-modern-mode)
    :ensure t
    :config
    (setq
     ;; Edit settings
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     ;; Appearance
     org-modern-radio-target    '("❰" t "❱")
     org-modern-internal-target '("↪ " t "")
     org-modern-block-name
  	 '((t . t)
  	   ("src" "ϰ" "ϰ"))
     org-modern-progress t
     org-modern-statistics nil
     org-modern-todo t
     org-modern-todo-faces (quote (("TODO" :background "indian red" :foreground "white" :weight bold)
  																 ("NEXT" :background "sky blue" :foreground "black" :weight bold)
  																 ("WAIT" :background "olive drab" :foreground "black" :weight bold)
  																 ("DONE" :background "pale green" :foreground "black" :weight bold)
  																 ("CNCL" :background "dark red" :foreground "white" :weight bold)))
     org-modern-priority t
     org-modern-priority-faces (quote ((?A :background "red"
  																				 :foreground "black")
  																		 (?B :background "dark orange"
  																				 :foreground "black")
  																		 (?C :background "tan"
  																				 :foreground "black")))
     org-modern-tag t
     org-modern-timestamp nil
     org-modern-statistics t
     ;; org-modern-table t
     org-modern-tag-faces (quote (("@coding" :background "#d60000" :foreground "#000000")
  																("@personal" :background "#e67c73" :foreground "#000000")
  																("@article" :background "#0b8043" :foreground "#000000")
  																("@mathematics" :background "#bc8f8f" :foreground "#000000")
                                  ("blockchain" :background "#f5511d" "#000000")
  																("solana" :background "#DC1FFF" :foreground "#000000")
  																("rust" :background "#CE412B" :foreground "#000000")
  																("go" :background "#00bfff" :foreground "#00000")
  																("exams" :background "#8e24aa" :foreground "#000000")))
     org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
     org-modern-hide-stars " "
     org-modern-keyword "‣"
     org-modern-table t))
(global-org-modern-mode t)

(use-package org-habit
  :after org
  :ensure nil
  :straight (:type built-in)
  :init
  ;;(add-to-list 'org-modules 'org-habit)
  (progn
    (custom-set-faces
     '(org-habit-clear-face
    	 ((t (:background "pale green"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))

     '(org-habit-clear-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-alert-future-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-alert-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-overdue-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-overdue-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-ready-face
    	 ((t (:background "pale green"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-ready-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     ))
  :config
  (load "~/.emacs.d/lisp/my-org-habit")
  (setq org-habit-following-days 1
    		org-habit-preceding-days 7
    		org-habit-show-habits nil
    		org-habit-show-all-today t
    		org-habit-graph-column 67
    		org-habit-overdue-glyph ?○
    		org-habit-alert-glyph ?○
    		org-habit-today-glyph ?○
    		org-habit-completed-glyph ?●
              org-habit-ready-future-glyph ?⬡
    		org-habit-show-done-always-green t)

  (defun toggle-org-habit-show-all-today ()
    "Toggle the value of `org-habit-show-all-today' between t and nil."
    (interactive)
    (setq org-habit-show-all-today (not org-habit-show-all-today))
    (message "org-habit-show-all-today is now %s"
    				 (if org-habit-show-all-today "nil" "t"))
    (org-agenda-refresh))

  (define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today))

(use-package org-habit-stats
  :ensure t
  :config
  (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
  (add-hook 'org-agenda-mode-hook
    				(lambda () (define-key org-agenda-mode-map "Z" 'org-habit-stats-view-next-habit-in-agenda))))

(defun org-habit-streak-count ()
  (goto-char (point-min))
  (while (not (eobp))
    (when (get-text-property (point) 'org-habit-p)
      (let ((streak 0)
            (counter (+ org-habit-graph-column org-habit-preceding-days)))
        (move-to-column counter)
        (while (and (>= counter org-habit-graph-column)
                    (= (char-after (point)) org-habit-completed-glyph))
          (setq streak (1+ streak))
          (setq counter (1- counter))
          (backward-char 1))
        (end-of-line)
        (insert (format "[🔥 %d]" streak))))
    (forward-line 1)))

(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

;;(frostyx/guix :install "alsa-utils")
  
(use-package sound-wav
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package powershell
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package org-pomodoro
  :ensure t
	:bind (("C-c k" . my/org-pomodoro))
	:config
	(setq org-pomodoro-audio-player (or (executable-find "aplay") (executable-find "afplay"))
        org-pomodoro-play-sounds t           ; Determines whether soudns are played or not
				org-pomodoro-keep-killed-pomodoro-time t
				org-pomodoro-format " %s"
				org-pomodoro-short-break-format " Short Break %s"
				org-pomodoro-long-break-format  " Long Break %s"
				;; org-pomodoro-finished-sound-p t
        ;; org-pomodoro-start-sound "/home/vberezhnev/.emacs.d/sounds/bell.mp3"

        org-pomodoro-start-sound-p t         ; Determine whether to play a sound when a pomodoro started
        org-pomodoro-start-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)
        org-pomodoro-length 40                ; The length of a pomodoro in minutes

        org-pomodoro-finished-sound-p t      ; Determines whether to play a sound when a pomodoro finished
        org-pomodoro-finished-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

        org-pomodoro-manual-break t          ; Whether the user needs to exit manually from a running pomodoro to enter a break
        org-pomodoro-overtime-sound-p t      ; Determines whether to play a sound when a pomodoro starts to run overtime
        org-pomodoro-overtime-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

				org-pomodoro-length 40
				org-pomodoro-short-break-length 5
				org-pomodoro-long-break-length 15
				org-pomodoro-long-break-frequency 3
				;;org-pomodoro-expiry-time 30
        ;;org-pomodoro-clock-break t           ; Whether to clock time during breaks
				))

(defun my/org-clock-get-clock-string ()
  (concat " " org-clock-heading))

(setq spaceline-org-clock-format-function 'my/org-clock-get-clock-string)

;; (set-face-attribute 'org-pomodoro-mode-line nil :foreground my/green)
;; (set-face-attribute 'org-pomodoro-mode-line-overtime nil :foreground my/red)

(defun my/org-pomodoro ()
  (interactive)
  (org-pomodoro '(4)))

(use-package org-timed-alerts
  :straight (:host github
             :repo "legalnonsense/org-timed-alerts"
             :branch "master" :files ("*.el" "out"))
  :after (org)
  :custom
  (org-timed-alerts-alert-function #'alert)
  (org-timed-alerts-tag-exclusions nil)
  (org-timed-alerts-default-alert-props nil)
  (org-timed-alerts-warning-times '(-30 -15 -5))
  (org-timed-alerts-agenda-hook-p t)
  (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time"))
  :config
  (add-hook 'org-mode-hook #'org-timed-alerts-mode))

;; ────────────────────────────── Prettify Symbols ─────────────────────────────
;; Beautify Org Checkbox Symbol
(defun ma/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(add-hook 'org-mode-hook 'ma/org-buffer-setup)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(evil-leader/set-key
	"nl" 'org-roam-buffer-toggle
	"nf" 'org-roam-node-find
	"ni" 'org-roam-node-insert
	"nc" 'org-roam-capture
	"nt" 'org-roam-tag-add
	"nr" 'org-roam-ref-add
	"nj" 'org-roam-dailies-capture-today
	"ng" 'org-id-get-create
  "nb" 'orb-insert-link)

(use-package sqlite3
  :ensure t)
(use-package emacsql
  :ensure t)

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c g" . org-id-get-create)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n b" . orb-insert-link)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("a" "Atomic note (with source)" plain (file "~/Org/Templates/Atomic note.org")
      :if-new
      (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org" "#+startup: latexpreview\n#+date: %U\n#+title: ${title}\n")
      :unnarrowed t)

     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
      :unnarrowed t)

     ("r" "Bibliography reference" plain (file "~/Org/Templates/Bibliography reference.org") ; <-- template store in a separate file
      :target
      (file+head "bibliography/references/${citekey}.org" "#+title: ${title}\n#+date: %U")
      :unnarrowed t)))

  (org-roam-capture-ref-templates
   '(("r" "ref" plain
      "%?"
      :target (file+head "web/${slug}.org"
                         "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n%(zp/org-protocol-insert-selection-dwim \"%i\")")
      :unnarrowed t)
     ("i" "incremental" plain
      "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
      :target (file+head "web/${slug}.org"
                         "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
      :unnarrowed t
      :empty-lines-before 1)))

  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

  (org-roam-dailies-capture-templates
      '(("d" "Дневник продуктивности - утро" plain (file "~/Org/Templates/journal/Morning.org")
        :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

        ("D" "Дневник продуктивности - вечер" plain (file "~/Org/Templates/journal/Evening.org")
        :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

        ("j" "Мысли" plain "* %U"
         :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))))
  :config
  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
        '("title" "citekey"  "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  ;; (setq org-roam-database-connector 'sqlite)
  (org-roam-db-autosync-mode)
                                        ; Show +FILETAG in node list
                                        ; https://github.com/org-roam/org-roam/commit/6f5d65abd9e707b3fdb10092a9fef3b739e143dd
  (setq fill-prefix "")  ;; see https://emacs.stackexchange.com/a/38943/12999
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:60}" 'face 'org-tag)))

  ;;for org-roam-buffer-toggle
  ;;Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  )

(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :demand t
  :config (org-roam-timestamps-mode)
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t))

(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

        Normally, insert the headline’s title using the ’#title:’ file-level property
        and delete the Org-mode headline. However, if the current headline has a
        Org-mode properties drawer already, keep the headline and don’t insert
        ‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
        ‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find 'other-window title nil)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(defun org-roam-insert-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

        Normally, insert the headline’s title using the ’#title:’ file-level property
        and delete the Org-mode headline. However, if the current headline has a
        Org-mode properties drawer already, keep the headline and don’t insert
        ‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
        ‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find 'other-window title nil)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-readwise
  :quelpa (org-readwise :fetcher github :repo "CountGreven/org-readwise")
  :config
  ;; Ensure auth-source is configured to find your Readwise token
  (setq auth-sources '("~/.authinfo"))
  
  ;; Set the output location for your highlights (buffer or file)
  (setq org-readwise-output-location "~/Org/readwise-highlights.org")
  
  ;; Optionally set the debug level (0 = no debug, 1 = basic debug, 2 = detailed debug)
  (setq org-readwise-debug-level 1))

(use-package restclient
  :ensure t)

(use-package org-download
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-x p m"    . org-download-clipboard)
              ("C-x p o"    . org-download-image))
  :config
  (setq-default org-download-image-dir "./assets-org/"))

(use-package org-cliplink
  :ensure t
  :demand t
  :config
  (setq org-cliplink-max-length 800)
  (global-set-key (kbd "C-x p i") 'org-cliplink))

;; (evil-leader/set-key
;; "de" 'org-gtd-engage
;; "dr" 'org-gtd-engage-grouped-by-context
;; "dp" 'org-gtd-process-inbox
;; "c" 'org-gtd-organize)

(evil-leader/set-key
	;; "dc" 'org-gtd-capture
  "dc" (lambda () (interactive) (org-gtd-capture nil "i"))
	"de" 'org-gtd-engage
	"dp" 'org-gtd-process-inbox
	"dn" 'org-gtd-show-all-next
	"ds" 'org-gtd-review-stuck-projects)

(use-package org-gtd
  :ensure t
  :straight (org-gtd :type git
                     :host github
                     :repo "trevoke/org-gtd.el")
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  ;; (org-edna-use-inheritance t)
  ;; (org-gtd-update-ack "3.0.0")
	(org-gtd-areas-of-focus '("PERSONAL" "MERITRANK" "CODING" "EGE"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
	(org-gtd-clarify-show-horizons t)
	(org-gtd-horizons-file "horizons.org")
  :config
  (org-edna-mode)
  :bind (("C-c d c" . (lambda () (interactive) (org-gtd-capture nil "i")))
				 ("C-c d e" . org-gtd-engage)
				 ("C-c d r" . org-gtd-engage-grouped-by-context)
				 ("C-c d p" . org-gtd-process-inbox)
				 :map org-gtd-clarify-map
				 ("C-c c" . org-gtd-organize)))

(use-package org-clock-budget
    :quelpa (org-clock-budget
        :fetcher github
        :repo "Fuco1/org-clock-budget"
        :branch "master")
		:ensure t
    :config
    (setq org-clock-budget-daily-budgetable-hours 10)
    (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week))))

(defun org-dblock-write:time-requirements (params)
  "Generate a table showing daily time requirements and progress for categories."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         (categories '("EGE" "MERITRANK" "CODING"))
         (today-start (format-time-string "%Y-%m-%d"))
         (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))
    
    ;; Создаем заголовок таблицы с фиксированной шириной столбцов
    (insert "| Category   | Required | Actual  | Progress  |\n")
    (insert "|------------+----------+---------+-----------|\n")
    
    (dolist (category categories)
      (let ((required 0.0)
            (actual 0.0))
        ;; Находим требуемое время
        (org-map-entries
         (lambda ()
           (let* ((cat (org-entry-get (point) "CATEGORY"))
                  (req (org-entry-get (point) required-property)))
             (when (and req (string= cat category))
               (setq required (string-to-number req)))))
         nil 'file)
        
        ;; Вычисляем фактическое время
        (setq actual (/ (float (org-clock-sum today-start today-end
                                             (lambda () 
                                               (string= (org-entry-get nil "CATEGORY") 
                                                      category))))
                       60.0))
        
        ;; Вычисляем прогресс
        (let ((progress (if (> required 0.0)
                          (* 100.0 (/ actual required))
                        0.0)))
          ;; Используем фиксированную ширину для каждого столбца
          (insert (format "| %-10s | %8.1f | %7.1f | %8.1f%% |\n"
                         category required actual progress)))))
    
    ;; Добавляем нижний разделитель
    (insert "|------------+----------+---------+-----------|")))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

(use-package timeblock
  :init
  (unless (package-installed-p 'timeblock)
    (package-vc-install
     '(timeblock
       :vc-backend Git
       :url "https://github.com/ichernyshovvv/timeblock.el"
       :branch "master"))))

(use-package org-timeblock
  :straight (org-timeblock :type git
 													 :host github
 													 :repo "ichernyshovvv/org-timeblock")
  :demand t
  :bind
  (:map global-map
 				("C-c s" . org-timeblock))
  :config
  (setq org-now-location '("~/Org/agenda/Calendar.org")
        org-timeblock-inbox-file "/home/berezhnev/Org/agenda/Calendar.org"
        org-timeblock-n-days-view 3))

;; -*- lexical-binding: t; -*- 

(require 'org) ;; for `org-read-date'

(let* ((date (parse-time-string "2024-11-08 00:00"))
       (entries
        (list (list
               (cons 'start (parse-time-string "2024-11-08 10:00"))
               (cons 'end (parse-time-string "2024-11-08 11:00"))
               (cons 'title "Block 1"))
              (list
               (cons 'start (parse-time-string "2024-11-08 09:30"))
               (cons 'end (parse-time-string "2024-11-08 11:00"))
               (cons 'title "Block 2"))
              (list
               (cons 'start (parse-time-string "2024-11-08 12:00"))
               (cons 'end (parse-time-string "2024-11-08 17:00"))
               (cons 'title "Block 4"))
              (list
               (cons 'start (parse-time-string "2024-11-08"))
               (cons 'end nil)
               (cons 'title "All-day Block 3")))))
  (timeblock-insert-column
   entries date 200 350
   :show-date t :show-all-day-entries t
   :scope '(6 . 24)
   :keymap (let ((map timeblock-column-map))
             (keymap-set map "e" #'timeblock-reschedule)
             (keymap-set map "<drag-mouse-1>" #'timeblock-drag-n-drop)
             map)))
 
(defun timeblock-reschedule ()
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (entries (dom-attr svg 'entries))
              (date (encode-time (dom-attr svg 'date)))
              (block-id (dom-attr (timeblock-get-selected svg) 'id))
              (entry (nth (string-to-number block-id) entries)))
    (setf (alist-get 'start entry)
          (decode-time (org-read-date t t nil "Start: " date)))
    (setf (alist-get 'end entry)
          (decode-time (org-read-date t t nil "End: " date)))
    (timeblock-redisplay-column)))

(defun timeblock-drag-n-drop (event)
  "Draw a line from the start of EVENT to its end."
  (interactive "e")
  (when-let* ((start (posn-object-x-y (event-start event)))
              (end (posn-object-x-y (event-end event)))
              (svg (get-text-property (point) 'dom))
              (entries (dom-attr svg 'entries))
              (block-id
               (dom-attr
                (timeblock-block-at-position svg (car start) (cdr start)) 'id))
              (entry (nth (string-to-number block-id) entries))
              (hour (timeblock-hour-at-position svg (cdr end))))
    (let* ((start-ts (alist-get 'start entry))
           (end-ts (alist-get 'end entry))
           (duration (and end-ts (timeblock-time-diff end-ts start-ts)))
           (new-start-ts (timeblock-time-apply start-ts :hour hour :minute 0))
           (new-end-ts (and duration
                            (timeblock-time-inc 'minute duration new-start-ts))))
      (setf (alist-get 'start entry) new-start-ts)
      (setf (alist-get 'end entry) new-end-ts)
      (timeblock-redisplay-column))))

(cl-defun timeblock-time-apply (time &key second minute hour day month year)
  "Return new timestamp based on TIME with new slot values from keys."
  (declare (indent 1))
  ;; This code is borrowed from `ts-apply' function which is part of ts.el
  ;; project written by Adam Porter
  (let ((time (copy-sequence time)))
    (and second (setf (decoded-time-second time) second))
    (and minute (setf (decoded-time-minute time) minute))
    (and hour (setf (decoded-time-hour time) hour))
    (and day (setf (decoded-time-day time) day))
    (and month (setf (decoded-time-month time) month))
    (and year (setf (decoded-time-year time) year))
    time))

(defun timeblock-time-diff (a b)
  "Return difference between times A and B in minutes."
  (when-let* ((a (encode-time a))
              (b (encode-time b)))
    (/ (time-convert (time-subtract a b) 'integer) 60)))

(defun timeblock-time-inc (slot value time)
  "Return a new time object based on TIME with its SLOT incremented by VALUE.

SLOT should be specified as a plain symbol, not a keyword."
  (let ((time (copy-sequence time)))
    (decoded-time-add time (make-decoded-time (intern (format ":%s" slot)) value))) )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(general-vmap
  :keymaps 'lisp-interaction-mode-map
  "<tab>" #'indent-region)

(use-package package-lint
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package protobuf-mode
	:ensure t)

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;;(setq rustic-format-on-save t)
  ;;(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))

  (use-package rust-playground
    :ensure t)

  ;; (use-package cargo
  ;;   :ensure t
  ;;   :if (executable-find "cargo")
  ;;   :after rust-mode
  ;;   :bind (:map cargo-minor-mode-map
  ;;               ("C-c C-t" . cargo-process-test)
  ;;               ("C-c C-b" . cargo-process-build)
  ;;               ("C-c C-c" . cargo-process-run))
  ;;   :config
  ;;   (add-hook 'rust-mode-hook 'cargo-minor-mode))
  )

(use-package go-mode
  :straight t
	:ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 2)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :after (company go-mode)
	:ensure t
  :straight t)

(use-package go-errcheck
  :after go-mode
	:ensure t
  :straight t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-buffer))
  :hook (;;(go-mode         . lsp-deferred)
         (rust-mode       . lsp-deferred)
         ;; (lisp            . lsp)
         (python-mode     . lsp-deferred)
         (c-mode          . lsp-deferred)
         ;; (c++-mode        . lsp-deferred)
         (js-mode         . lsp-deferred)
         ;; (solidity-mode   . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0) ;; BEFORE: 0.2
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  ;; (lsp-rust-analyzer-display-reborrow-hints t)

  :config
  (setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-signature-render-documentation nil)
	(with-eval-after-load 'lsp-mode
		(set-face-attribute 'lsp-face-highlight-read nil :underline nil))
	(setq lsp-enable-snippet t) ;; nil
	(setq lsp-lens-enable t) ;; nil
	(setq lsp-diagnostic-provider :none))

(use-package helm-lsp
  :ensure t)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq
   lsp-inlay-hints-mode t
   lsp-ui-doc-enable t
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 130         ; 150 (default) is too wide
   lsp-ui-doc-delay 0.1           ; 0.2 (default) is too naggy
   lsp-ui-doc-show-with-mouse t  ; don't disappear on mouseover
   ;; lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header t))

(use-package dockerfile-mode
  :ensure t)

(use-package format-all
  :ensure t
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") 'ian/format-code)
  (global-set-key (kbd "C-c C-f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

(setq display-line-numbers 'relative)

(use-package direnv
	:ensure t
	:config
	(direnv-mode))

(use-package gptel
  ;;:load-path "~/.emacs.d/gptel/"
  :ensure t
  :init
  (setq gptel-api-key (getenv "AIML_API"))
  (setq gptel-max-tokens 8024)
  :config
  (setq gptel-model 'gpt-4o
        gptel-backend
        (gptel-make-openai "AIMLAPI"
          :host "api.aimlapi.com"
          :endpoint "/chat/completions"
          :stream t
          :key gptel-api-key
          :models '(gpt-4o
                    gpt-4o-2024-08-06
                    gpt-4-turbo
                    chatgpt-4o-latest)))
  :bind (("M-s M-d" . gptel-context-add)
         ("M-s M-f" . gptel-add-file)
         ("M-s M-a" . gptel-menu)
         ("M-s M-r" . gptel--regenerate)
         ("M-s M-e" . gptel-rewrite)
         ("M-s M-s" . gptel)))

(use-package whisper
  :load-path "~/.emacs.d/lisp/whisper.el"
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
