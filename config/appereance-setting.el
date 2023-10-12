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
                    :height 130
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 130
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 130
                    :weight 'medium)
(set-frame-font "Iosevka" nil t)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Iosevka 13"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka 13"))
(add-to-list 'default-frame-alist '(font . "Iosevka 13"))

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
  ;; :custom-face
  ;; (cursor ((t (:background "BlanchedAlmond"))))
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

;; (use-package modus-themes)
;; (use-package timu-rouge-theme)
(use-package gruvbox-theme)
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
;; (change-theme 'doom-one-light 'doom-one)
(change-theme 'doom-gruvbox-light 'gruvbox-dark-soft)

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
;; (use-package volatile-highlights
;;   :diminish
;;   :commands volatile-highlights-mode
;;   :hook (after-init . volatile-highlights-mode)
;;   :custom-face
;;   (vhl/default-face ((nil (:foreground "#FF3333" :background "BlanchedAlmond"))))) ; "#FFCDCD"

;; ;; hl-numbers
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
  :hook ((prog-mode . display-line-numbers-mode)))
  ;; :config
  ;; (defun display-line-numbers-scale-linum ()
  ;;   (set-face-attribute 'line-number nil :height 0.6 :background (face-background 'solaire-default-face))
  ;;   (set-face-attribute 'line-number-current-line nil :height 0.6 :background (face-background 'solaire-default-face)))
  ;; (display-line-numbers-scale-linum)
  ;; (setq display-line-numbers-width 3)

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
  (olivetti-body-width 100)
  :delight " ⊗") ; Ⓐ ⊛

;;________________________________________________________________
;;    Modeline
;;________________________________________________________________
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-major-mode-color-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-time-icon nil
        doom-modeline-battery nil
        doom-modeline-time nil
        doom-modeline-modal nil)
  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil))

;;   (set-face-attribute 'mode-line nil
;;                     :background "#353644"
;;                     :foreground "white"
;;                     :box '(:line-width 8 :color "#353644")
;;                     :overline nil
;;                     :underline nil)

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#565063"
;;                     :foreground "white"
;;                     :box '(:line-width 8 :color "#565063")
;;                     :overline nil
;;                     :underline nil)

(provide 'appereance-setting)
