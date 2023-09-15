;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________
(set-frame-parameter (selected-frame) 'alpha '(97 .97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;;________________________________________________________________
;;    Setup fonts
;;________________________________________________________________
(set-face-attribute 'default t
                    :font "Hack" ;; Iosevka, Input, Terminess, Nerd, Font Propo
                    :height 120
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Hack"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Hack"
                    :height 120
                    :weight 'medium)
(set-frame-font "Hack" nil t)

;; Setup fonts
;; (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 130)
;; (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Mono")
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 150)

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
(use-package modus-themes)
(use-package theme-changer
  :ensure t
  :demand t
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88))
(require 'theme-changer)
(change-theme 'modus-operandi 'modus-vivendi)
;; (use-package gruvbox-theme)
;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package display-line-numbers
  :ensure nil
  :commands (display-line-numbers-scale-linum)
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (defun display-line-numbers-scale-linum ()
    (set-face-attribute 'line-number nil :height 0.6 :background (face-background 'solaire-default-face))
    (set-face-attribute 'line-number-current-line nil :height 0.6 :background (face-background 'solaire-default-face)))
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
