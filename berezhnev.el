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

(pixel-scroll-precision-mode)

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

;; (change-theme 'tsdh-light 'doom-xcode)
(change-theme 'doom-one-light 'doom-xcode)

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

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package gitignore-templates
  :ensure t)

(setq gitignore-templates-api 'github)

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

(use-package memento-mori
  :ensure t
  :custom (memento-mori-mementos
           '(("%.5Y years old" :since "2007-18-09")))
  :config (memento-mori-mode))

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
  :config
  (setq telega-use-docker t))

(use-package elfeed
  :if workstation-p
  :ensure t
  :commands (elfeed)
  :config
  (setq-default elfeed-search-filter ""))

(use-package elfeed-org
  :if workstation-p
  :ensure t
  :config
  (elfeed-org))

(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

(setq elfeed-search-filter "+work")

(setq my/elfeed-update-timer
  (run-at-time nil (* 1 60 60 24) #'elfeed-update))

(defun elfeed-apply-for-all (f)
  (interactive)
  (mark-whole-buffer)
  (f))

(defun elfeed-mark-all-as-read ()
  (elfeed-apply-for-all elfeed-search-untag-all-unread))

(with-eval-after-load "elfeed-search"
  (evil-define-key*
    'normal elfeed-search-mode-map
    "RET" #'elfeed-search-show-entry
    "o" #'elfeed-search-browse-url
    "r" #'elfeed-search-fetch
    "S" #'elfeed-unjam))

(with-eval-after-load "elfeed-show"
  (evil-define-key*
    'motion elfeed-show-mode-map
    "gb" #'elfeed-show-visit
    "gj" #'elfeed-show-next
    "gk" #'elfeed-show-prev))

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

(notifications-notify
 :app-name "EDNC"
 :title "1st test"
 :body "hello, world"
 :urgency "normal")

(defun frostyx/autoscroll ()
  (set (make-local-variable 'window-point-insertion-type) t))

(add-hook 'ednc-view-mode-hook 'frostyx/autoscroll)

(use-package taxy-magit-section
	:ensure t)

(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :demand t
  :config
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-web
  :ensure t)

(setq httpd-port 8090)
(setq httpd-host "0.0.0.0")

(ignore-error
  (elfeed-web-start))

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

(use-package org-ql
    :ensure t)

(if (and workstation-p (fboundp #'circe-tracking-get-face))
	(progn
	  (setq tracking-get-face-function #'circe-tracking-get-face)
	  (set-face-attribute 'circe-tracking-channel-face nil :foreground my/white)
	  (set-face-attribute 'circe-tracking-query-face nil :foreground my/blue)))

  (defun doom-modeline--frostyx-org-agenda-update ()
    (when doom-modeline-frostyx-org-agenda
      (let ((inbox-count (length (org-ql-select (org-agenda-files)
                                   '(todo "IN-PROGRESS")
                                   :action #'org-get-heading)))
            (soon-count (length (org-ql-select (org-agenda-files)
                    '(todo "NEXT")
                    :action #'org-get-heading))))
        (setq doom-modeline--frostyx-org-agenda-count
              (format " DOING: %s |  NEXT: %s"
                      ;; TODO Don't abuse circe face here
                      (propertize (number-to-string inbox-count)
                                  'face `(:foreground "purple"))
                      (propertize (number-to-string soon-count)
                                  'face `(:foreground "purple")))))))

  (defvar doom-modeline--frostyx-org-agenda-count "")
  (doom-modeline-def-segment frostyx-org-agenda-count
    ;; (when (equal major-mode 'org-agenda-mode)
    ;;   (if (string= doom-modeline--frostyx-org-agenda-count "")
    ;;       (doom-modeline--frostyx-org-agenda-update))
    ;;   (format-mode-line doom-modeline--frostyx-org-agenda-count))
    (format-mode-line doom-modeline--frostyx-org-agenda-count))


  (defvar doom-modeline--frostyx-org-agenda-timer nil)
  (doom-modeline-add-variable-watcher
   'doom-modeline-frostyx-org-agenda
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq doom-modeline-frostyx-org-agenda val)
       (doom-modeline-frostyx-org-agenda-timer))))


  (defvar doom-modeline--frostyx-org-agenda-timer nil)
  (defun doom-modeline-frostyx-org-agenda-timer ()
    (if (timerp doom-modeline--frostyx-org-agenda-timer)
        (cancel-timer doom-modeline--frostyx-org-agenda-timer))
    (setq doom-modeline--frostyx-org-agenda-timer
          (and doom-modeline-frostyx-org-agenda
               (run-with-idle-timer
                10 10 #'doom-modeline--frostyx-org-agenda-update))))

  (defvar doom-modeline-frostyx-org-agenda t)
  (doom-modeline-frostyx-org-agenda-timer)

  (doom-modeline-def-segment circe-lagmon t)
  (doom-modeline-def-segment circe-track t)
  (doom-modeline-def-segment frostyx-org-agenda-count t)

  (setq frostyx/doom-modeline-middle-segments
        '(workspace-name window-number modals matches follow buffer-info-simple
  											 major-mode remote-host word-count parrot selection-info
  											 ))

  (setq frostyx/doom-modeline-right-segments
        '(frostyx-org-agenda-count compilation objed-state persp-name battery grip gnus
  										github debug repl lsp minor-modes input-method indent-info
  										buffer-encoding process vcs time circe-lagmon circe-track
  										buffer-position))
  (doom-modeline-def-modeline 'main
    frostyx/doom-modeline-middle-segments
    frostyx/doom-modeline-right-segments)

(doom-modeline-def-modeline 'helm
  '(helm-buffer-id helm-number helm-follow helm-prefix-argument)
  '(helm-help time))

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
  (company-mode . frostyx/company-mode-hook))

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
  :ensure nil
  ;; :defer t
  ;; :after org
  ;; :demand t
  ;; :delight org-mode "✎"
  ;; :hook ((org-mode . prettify-symbols-mode)
  ;;        (org-mode . visual-line-mode)
  ;;        (org-mode . variable-pitch-mode))
  :bind (("C-c l"               . org-store-link)
         ("C-c c"               . org-capture)
         ("C-c f"               . org-footnote-new)))
;;   :config

;; (setq org-modules
;; 	'(org-crypt
;;         org-bookmark
;;         org-eshell
;;         org-irc))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Setup fonts for org-mode
;; (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
;; (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(define-key global-map (kbd "C-c u") #'calendar)

;; (use-package org-pomodoro
;;   :straight (:host github :repo "marcinkoziej/org-pomodoro" :branch "master")
;;   :bind (("C-c k"               . org-pomodoro))
;;   :config
;;   ;; First of all you sould install aplay or afplay
;;   (use-package sound-wav
;;     :ensure t
;;     :demand t) ;; dep for org-pomodoro
;;   (use-package powershell
;;     :ensure t
;;     :demand t) ;; dep for org-pomodoro
;;   (setq org-pomodoro-length 35
;;         org-pomodoro-short-break-length 5
;;         org-pomodoro-long-break-length 15
;;         org-pomodoro-long-break-frequency 4
;;         org-pomodoro-play-sounds 1

;;         org-pomodoro-finished-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"
;;         org-pomodoro-long-break-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"
;;         org-pomodoro-short-break-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"))




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

;; https://orgmode.org/worg/org-hacks.html
;; work with org-agenda dispatcher [c] "Today Clocked Tasks" to view today's clocked tasks.
;; (defun org-agenda-log-mode-colorize-block ()
;;   "Set different line spacing based on clock time duration."
;;   (save-excursion
;;     (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
;;                                  ('light
;;                                   (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
;;                                  ('dark
;;                                   (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
;;            pos
;;            duration)
;;       (nconc colors colors)
;;       (goto-char (point-min))
;;       (while (setq pos (next-single-property-change (point) 'duration))
;;         (goto-char pos)
;;         (when (and (not (equal pos (point-at-eol)))
;;                    (setq duration (org-get-at-bol 'duration)))
;;           ;; larger duration bar height
;;           (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
;;                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;;             (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
;;             (setq colors (cdr colors))
;;             (overlay-put ov 'line-height line-height)
;;             (overlay-put ov 'line-spacing (1- line-height))))))))

;; (add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)
;; )

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
 org-log-reschedule t
 org-todo-keyword-faces
 '(
   ("TODO" :background "indian red" :foreground "white" :weight bold)
   ("NEXT" :background "sky blue" :foreground "black" :weight bold)
   ("WAIT" :background "olive drab" :foreground "black" :weight bold)
   ("DONE" :background "pale green" :foreground "black" :weight bold)
   ("CNCL" :background "dark red" :foreground "white" :weight bold))
org-todo-keywords
'((sequence "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)"))) ; changed

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

(use-package org-agenda
  :ensure nil
  :straight (:type built-in)
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config
  (setq org-agenda-skip-scheduled-if-done nil ; changed
        org-agenda-skip-deadline-if-done nil ; changed
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
																				; (setq-default org-icalendar-include-todo t)
				org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        ;; org-agenda-time-grid (quote ((daily today remove-match)
        ;; 			     (800 1200 1600 2000)
        ;; 			     "......" "----------------"))
        org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t% s") ;; use "%i %-12:c%?-12t%b% s" to display path
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c"))
				org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date)))
				org-default-notes-file "~/Org/agenda/Notes.org"
				org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
													 )) ;; "~/Org/agenda/Calendar.org"

  ;; (setq org-agenda-clockreport-parameter-plist
  ;;       (quote (:link t :maxlevel 5 :fileskip t :compact t :narrow 80)))

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.1)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-custom-commands
        '(("z" "Getting Things Done (GTD)"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-include-deadlines nil)
                        (org-agenda-prefix-format '((agenda . "%i %?-12t% s") ;; use "%i %-12:c%?-12t%b% s" to display path
                                                    (todo . " %i %-12:c")
                                                    (tags . " %i %-12:c")
                                                    (search . " %i %-12:c")))

                        (org-agenda-files '("~/Org/agenda/PlanAhead.org" "~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Schedule"
																	:time-grid t)
                           (:name "School / exams"
																	:and (:tag "school" :deadline future)
																	:face (:background "yellow" :foreground "black"))
                           (:name "Today"
																	:scheduled today
																	:face (:background "medium sea green" :foreground "white"))
                           (:name "Deadline today"
																	:deadline today
																	:face (:background "black" :foreground "white"))
                           (:name "Passed deadline"
																	:deadline past
																	:face (:background "firebrick"))
                           (:name "Future deadline"
																	:deadline future
																	:face (:background "dark slate blue"))))))

            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format "  %?-12t% s")
                         (org-agenda-entry-text-mode t)
                         (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")) ;; "~/Org/agenda/GTD/Projects.org"
                         (org-super-agenda-groups
                          '((:name "Tasks ready to actions"
																	 :children t
																	 :todo "NEXT")))))

            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))

          ("v" "Reading view (by tags)"
           ((todo "" ((org-agenda-overriding-header "")
                      (org-agenda-start-with-log-mode '(closed))
                      (org-agenda-files '("~/Org/agenda/ReadAhead.org" "~/Org/agenda/Reading-list.org"))
                      (org-super-agenda-groups
                       '((:name "In progress / Reading"
																:face (:background "dark slate blue")
																:todo ("READING" "TODO")))

                       (:name "Should read"
															:and (:todo "IN-PLANS"))

                       (:name "On Zettelkasting"
															:todo "ZETTEL")

                       (:name "Paused reading"
															:todo "PAUSED")

                       (:name "Planned to read"
															:todo "NEXT-TO-READ")

                       (:name "Today deadline"
															:deadline today
															:face (:background "black"))
                       (:name "Passed deadline"
															:and (:deadline past)
															:face (:background "firebrick"))

                       (:name "Read books"
															:todo "READ")
                       (:name "Dropped books"
															:todo "DROPPED")
                       (:name "All books"
															:and (:tag "books" :todo "IN-PLANS")))))))

          ("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/Habits.org" "~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Everytime habits"
																	:tag "everytime")
													 (:name "Morning habits"
																	:tag "morning")
													 (:name "Day habits"
																	:tag "day")
													 (:name "Evening habits"
																	:tag "evening")
													 (:name "Sport habits"
																	:tag "sport")
													 (:name "Challenges"
																	:tag "challenge")
													 (:discard (:anything))
													 (:discard (:not (:tag "habits")))))))))))

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
   (latex      . t)))

(setq org-tag-alist
      '(
				("@article" . ?a)
        ("@personal" . ?P)
        ("@programming" . ?p)
        ("@mathematics" . ?m)
        ("@english" . ?e)
				("@work" . ?w)))

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
   org-modern-timestamp t
   org-modern-statistics t
   ;; org-modern-table t
   org-modern-tag-faces (quote (("@programming" :background "#d60000" :foreground "#000000")
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
   org-modern-keyword "‣"))

(use-package org-habit
  :after org
  :ensure nil
  :straight (:type built-in)
  :init
  (add-to-list 'org-modules 'org-habit)
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 7
        org-habit-show-all-today nil
        org-habit-show-habits t
        org-habit-graph-column 67)

  (defun toggle-org-habit-show-all-today ()
		"Toggle the value of `org-habit-show-all-today' between t and nil."
		(interactive)
		(setq org-habit-show-all-today (not org-habit-show-all-today))
		(message "org-habit-show-all-today is now %s"
						 (if org-habit-show-all-today "nil" "t"))
		(org-agenda-refresh))

  (define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today))

;;   (use-package org-habit-stats
;;     :config
;; (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
;; (add-hook 'org-agenda-mode-hook
;;               (lambda () (define-key org-agenda-mode-map "Z" 'org-habit-stats-view-next-habit-in-agenda)))))

(use-package org-pomodoro
  :ensure t)

(setq org-pomodoro-keep-killed-pomodoro-time t)

(setq org-pomodoro-format " %s")
(setq org-pomodoro-short-break-format " Short Break %s")
(setq org-pomodoro-long-break-format " Long Break %s")

(defun my/org-clock-get-clock-string ()
  (concat " " org-clock-heading))

(setq spaceline-org-clock-format-function 'my/org-clock-get-clock-string)

(setq org-pomodoro-start-sound-p t)
(setq org-pomodoro-finished-sound-p t)

(setq org-pomodoro-short-break-sound-p nil)
(setq org-pomodoro-long-break-sound-p nil)

(setq org-pomodoro-length 30)
(setq org-pomodoro-short-break-length 0)
(setq org-pomodoro-long-break-length 0)
(setq org-pomodoro-long-break-frequency 1)
(setq org-pomodoro-expiry-time 30)

(defun my/org-pomodoro ()
  (interactive)
  (org-pomodoro '(4)))

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
	"ng" 'org-id-get-create)

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
     ("t" "Thought" plain "%?"
      :if-new (file+head "thoughts/%<%Y-%m-%d-%H:%M>--thought-${slug}.org" "#+title: ${title}\n#+filetags: :Thought:\n#+date: %U\n\n\n* See also:\n+ ")
      :unnarrowed t)

     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
      :unnarrowed t)

     ("p" "Project" plain (file "~/Org/Templates/Project.org")
      :if-new (file+head "projects/%<%Y-%m-%d-%H:%M>--project-${slug}.org" "#+title: ${title}\n#+filetags: :Project:\n#+date: %U\n\n")
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
   '(
     ("m" "Morning diary" plain (file "~/Org/Templates/journal/Morning.org")
      :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))
     ("u" "Quotetions diary" entry "** Quotation of the day (%U)‎\n\n#+begin_quote\n%^{Quote}\n#+end_quote\n+ Author: *%^{Author of quote}*\n\n* Reflections about this quote"
      :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>-quote.org" "#+title: %U\n\n"))

     ("d" "Default diary" entry "** Default (%U): «%?»‎\n\n"
      :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "** %U\n#+title: %U\n\n"))

     ("e" "Evening diary" plain (file "~/Org/Templates/journal/Evening.org")
      :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))))
  :config
  ;; Org-noter integration with org-roam-bibtex
  ;; (setq orb-preformat-keywords
  ;;       '("title" "citekey"  "url" "author-or-editor" "keywords" "file")
  ;;       orb-process-file-keyword t)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-database-connector 'sqlite)
  (org-roam-db-autosync-mode)
                                        ; Show +FILETAG in node list
                                        ; https://github.com/org-roam/org-roam/commit/6f5d65abd9e707b3fdb10092a9fef3b739e143dd
  (setq fill-prefix "")  ;; see https://emacs.stackexchange.com/a/38943/12999
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
  ;; Customize the org-roam buffer
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*org-roam\\*"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 0.33)
  ;;                (window-height . fit-window-to-buffer)))

  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

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

(use-package deft
  :ensure t
  :bind ("<f9>" . deft)
  :demand t
  :init
  (setq deft-directory "~/Org/Notes"
				deft-text-mode 'org-mode
        deft-extensions '("org" "txt" "md")
        deft-default-extension "org"
        deft-recursive t
        deft-new-file-format "%Y-%m-%dT%H%M"))

(evil-leader/set-key
	"fn" 'deft-new-file
	"fl" 'deft)

(use-package restclient
  :ensure t)

(use-package toc-org
  :ensure t
  :config
  (if (require 'toc-org nil t)
      (progn
        (add-hook 'org-mode-hook 'toc-org-mode))
    (warn "toc-org not found")))

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

(evil-leader/set-key
"dc" 'org-gtd-capture
"de" 'org-gtd-engage
"dr" 'org-gtd-engage-grouped-by-context
"dp" 'org-gtd-process-inbox)

(use-package org-gtd
  :ensure t
  :defer t
  :straight (org-gtd :type git
                     :host github
                     :repo "trevoke/org-gtd.el")
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  ;; (org-edna-use-inheritance t)
  ;; (org-gtd-update-ack "3.0.0")
  ;; (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  :config
  (org-edna-mode)
  :bind (;; ("C-c d c" . org-gtd-capture)
				 ;; ("C-c d e" . org-gtd-engage)
				 ;; ("C-c d r" . org-gtd-engage-grouped-by-context)
				 ;; ("C-c d p" . org-gtd-process-inbox)
				 :map org-gtd-clarify-map
				 ("C-c c" . org-gtd-organize)))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

(use-package org-transclusion
  :after org
  :ensure t
  :config
  (define-key global-map (kbd "<f12>") #'org-transclusion-add)
  (define-key global-map (kbd "C-c t") #'org-transclusion-mode))

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
  (use-package cargo
    :ensure t
    :if (executable-find "cargo")
    :after rust-mode
    :bind (:map cargo-minor-mode-map
                ("C-c C-t" . cargo-process-test)
                ("C-c C-b" . cargo-process-build)
                ("C-c C-c" . cargo-process-run))
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode)))

(use-package graphql-mode
		:ensure t)

(use-package yaml-mode
	:ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
                ("C-c f" . lsp-format-buffer))
  :hook ((go-mode         . lsp-deferred)
         (rust-mode       . lsp-deferred)
         ;; (lisp            . lsp)
         (c-mode          . lsp-deferred)
         (c++-mode        . lsp-deferred)
         (js-mode         . lsp-deferred)
         (solidity-mode   . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.4)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

(setq lsp-headerline-breadcrumb-enable nil)

(setq lsp-signature-render-documentation nil)

(with-eval-after-load 'lsp-mode
  (set-face-attribute 'lsp-face-highlight-read nil :underline nil))

(setq lsp-enable-snippet t) ;; nil

(setq lsp-lens-enable t) ;; nil

(setq lsp-diagnostic-provider :none)

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
   lsp-ui-doc-delay 0.2           ; 0.2 (default) is too naggy
   lsp-ui-doc-show-with-mouse t  ; don't disappear on mouseover
   ;; lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-border (face-foreground 'default)
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-include-signature t
   lsp-ui-doc-header t))

(use-package dockerfile-mode
  :ensure t)

(setq display-line-numbers 'relative)
