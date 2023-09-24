;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;     APPEREANCE     ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(95 . 95) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;________________________________________________________________
;;    Setup fonts
;;________________________________________________________________
(set-face-attribute 'default t
                    :font "Iosevka" ;; Hack, Input, Terminess, Nerd, Font Propo
                    :height 120
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 120
                    :weight 'medium)
(set-frame-font "Iosevka" nil t)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Iosevka 12"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka 12"))
(add-to-list 'default-frame-alist '(font . "Iosevka 12"))

;; Changes certain keywords to symbols, such as lamda
(setq global-prettify-symbols-mode t)

(set-fontset-font t 'unicode "FontAwesome" nil 'prepend)
(use-package all-the-icons
  :demand t
  :ensure t
  :config
  ;; Make sure the icon fonts are good to go
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

;;________________________________________________________________
;;    Setup theme
;;________________________________________________________________
(use-package doom-themes
  :if window-system
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (load-theme 'doom-gruvbox t)
  (if (display-graphic-p)
      (progn
        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        (doom-themes-treemacs-config)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package gruvbox-theme)
;; (use-package modus-themes)
;; (use-package timu-rouge-theme)
(use-package theme-changer
  :ensure t
  :demand t
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88))
(require 'theme-changer)
;; (change-theme 'modus-operandi 'modus-vivendi)
;; (change-theme 'doom-gruvbox-light 'timu-rogue)
;; (change-theme 'doom-gruvbox-light 'gruvbox-dark-soft)
(change-theme 'doom-one 'gruvbox-one-light)

;;;;; hl-indent
(use-package highlight-indent-guides
  :demand t
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-auto-enabled t)
  ;; (highlight-indent-guides-character ?\┆) ;; Indent character samples: | ┆ ┊
  :commands highlight-indent-guides-mode
  :hook (prog-mode  . highlight-indent-guides-mode)
  :delight " ㄓ")

;;;;; hl-volatile
(use-package volatile-highlights
  :diminish
  :commands volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "BlanchedAlmond"))))) ; "#FFCDCD"

;;;;; hl-numbers
;; (use-package highlight-numbers
;;   :hook (prog-mode . highlight-numbers-mode))

;;;;; hl-todo
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package display-line-numbers
  :ensure nil
  :commands (display-line-numbers-scale-linum)
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  ;; (defun display-line-numbers-scale-linum ()
  ;;   (set-face-attribute 'line-number nil :height 0.6 :background (face-background 'solaire-default-face))
  ;;   (set-face-attribute 'line-number-current-line nil :height 0.6 :background (face-background 'solaire-default-face)))
  (display-line-numbers-scale-linum)
  (setq display-line-numbers-width 3))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config (which-key-mode))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

;;;;; olivetti
(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         ;; (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode))
  :custom
  (olivetti-body-width 90)
  :delight " ⊗") ; Ⓐ ⊛

(dolist (mode '(org-mode-hook ; Disable line numbers for some modes
                org-mode-agenda-hook
                elfeed-entry-hook
                elfeed-new-entry-hook
                elfeed-new-entry-parse-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                nov-mode-hook
                neotree-mode-hook
                ;; pdf-view-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;________________________________________________________________
;;    Modeline
;;________________________________________________________________
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  ;; :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  ;; (setq doom-modeline-bar-width 3
  ;;       doom-modeline-github nil
  ;;       doom-modeline-mu4e nil
  ;;       doom-modeline-persp-name nil
  ;;       doom-modeline-minor-modes nil
  ;;       doom-modeline-major-mode-icon nil
  ;;       doom-modeline-buffer-file-name-style 'relative-from-project
  ;;       ;; Only show file encoding if it's non-UTF-8 and different line endings
  ;;       ;; than the current OSes preference
  ;;       doom-modeline-buffer-encoding 'nondefault
  ;;       doom-modeline-default-eol-type)
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project
        ;; doom-modeline-bar-width 5
        doom-modeline-lsp t
        doom-modeline-env-version t
        doom-modeline-indent-info t
        doom-modeline-buffer-encoding nil
        doom-modeline-enable-word-count t
        doom-modeline-vcs-max-length 20
        doom-modeline-major-mode-color-icon t
        doom-modeline-time-icon t
        doom-modeline-battery t
        doom-modeline-time t
        doom-modeline-workspace-name t
        doom-modeline-env-version t
        doom-modeline-modal-modern-icon t
        doom-modeline-modal-icon t
        doom-modeline-modal t)
  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook 'magit-mode-hook
	     (defun +modeline-hide-in-non-status-buffer-h ()
	       "Show minimal modeline in magit-status buffer, no modeline elsewhere."
	       (if (eq major-mode 'magit-status-mode)
		   (doom-modeline-set-modeline 'magit)
		 (hide-mode-line-mode)))))

(provide 'appereance-setting)
