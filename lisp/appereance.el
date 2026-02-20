;;; appereance.el --- Appearance and UI configuration

;; General UI settings (unchanged, moved to top for clarity)
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

;; Line number settings
(setq display-line-numbers 'relative
      display-line-numbers-type 'visual
      display-line-numbers-width 2
      display-line-numbers-current-absolute t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Disable UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))


(defun system-theme-mode ()
  "Считывает текущую системную тему из файла theme-mode."
  (let ((file (concat (getenv "XDG_RUNTIME_DIR") "/theme-mode")))
    (when (file-exists-p file)
      (string-trim
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-detection-method 'custom)
  :config
  (setq auto-dark-custom-theme-detection
        (lambda ()
          (pcase (system-theme-mode)
            ("dark" 'dark)
            ("light" 'light)
            (_ nil))))
  (setq auto-dark-themes '((modus-vivendi) (leuven)))
  (auto-dark-mode))

(defun update-theme-from-system-file (_event)
  "Вызывается при изменении файла theme-mode."
  (pcase (system-theme-mode)
    ("dark" (auto-dark--set-theme 'dark))
    ("light" (auto-dark--set-theme 'light))))

(when (fboundp 'file-notify-add-watch)
  (file-notify-add-watch
   (concat (getenv "XDG_RUNTIME_DIR") "/theme-mode")
   '(change)
   #'update-theme-from-system-file))

(update-theme-from-system-file nil)

(use-package beacon
  :straight t)

;; Doom-modeline: Load on demand
(use-package doom-modeline
  :straight t
  ;; :commands (doom-modeline-mode)
  :hook (after-init . doom-modeline-mode) ;; Load after init to ensure UI is ready
  :config
  (doom-modeline-def-segment my-org-agenda
    "Show org agenda alerts in mode-line"
    (concat
     (when (> my/agenda-overdue 0)
       (propertize (format " 🔥%d" my/agenda-overdue)
                   'face '(:foreground "#ff6c6b")))
     (when (> my/agenda-today 0)
       (propertize (format " ⏳%d" my/agenda-today)
                   'face '(:foreground "#ECBE7B")))
     (when (> my/agenda-soon 0)
       (propertize (format " 📌%d" my/agenda-soon)
                   'face '(:foreground "#51afef")))))
  (display-battery-mode t)
  (display-time-mode t)
  (setq display-time-format "%H:%M"
        display-time-day-and-date t
        display-time-24hr-format t
        display-time-interval 60
        display-time-load-average nil
        doom-modeline-height 24
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
  ;; (doom-modeline-def-modeline 'my-custom-modeline
  ;;   '(bar buffer-info remote-host buffer-position word-count selection-info)
  ;;   '(time battery major-mode misc-info process vcs))
  ;; (doom-modeline-set-modeline 'my-custom-modeline t)
  )

;; (doom-modeline-def-modeline 'my-line
;;   '(bar my-org-agenda workspace-name window-number modals matches buffer-info)
;;   '(misc-info major-mode process vcs check))

;; (add-hook 'doom-modeline-mode-hook
;;           (lambda ()
;;             (doom-modeline-set-modeline 'my-line 'default)))


;; Ligature: Load for programming and eww modes
;; (use-package ligature
;;   :straight t
;;   :commands (global-ligature-mode ligature-mode)
;;   :hook (prog-mode . ligature-mode)
;;   :config
;;   (ligature-set-ligatures 't '("www"))
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&& "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\\\" "://"))
;;   (global-ligature-mode t))

;; Ultra-scroll: Load for scrolling commands
(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll" :branch "main")
  ;; :commands (ultra-scroll-mode)
  :demand t
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package gruvbox-theme
  :straight (:host github :repo "greduan/emacs-theme-gruvbox" :branch "master"))

;; Hide-mode-line: Load on demand
(use-package hide-mode-line
  :straight t
  :commands (global-hide-mode-line-mode hide-mode-line-mode))


;; (use-package dashboard
;;   :straight t
;;   :config
;;   (setq dashboard-display-icons-p t)
;;   (setq dashboard-heading-icons
;;         '((recents   . "nf-cod-file")
;;           (bookmarks . "nf-cod-bookmark")
;;           (projects  . "nf-cod-rocket")
;;           (agenda    . "nf-cod-calendar")))
;;   ;; (setq dashboard-show-shortcuts nil)
;;   ;; (setq dashboard-init-info "")
;;   (setq dashboard-center-content t)
;;   (setq dashboard-items '((recents . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda . 5)))
;;   (dashboard-setup-startup-hook))

;; (use-package doom-dashboard
;;   :load-path "~/.emacs.d/lisp/packages/doom-dashboard"
;;   :after (dashboard nerd-icons)
;;   :demand t
;;   :bind
;;   (:map dashboard-mode-map
;;         ("<remap> <dashboard-previous-line>" . widget-backward)
;;         ("<remap> <dashboard-next-line>" . widget-forward)
;;         ("<remap> <previous-line>" . widget-backward)
;;         ("<remap> <next-line>" . widget-forward)
;;         ("<remap> <right-char>" . widget-forward)
;;         ("<remap> <left-char>" . widget-backward))
;;   :custom
;;   (dashboard-banner-logo-title " ")
;;   (dashboard-page-separator "\n")
;;   (dashboard-startupify-list
;;    '(dashboard-insert-banner
;;      dashboard-insert-banner-title
;;      dashboard-insert-newline
;;      dashboard-insert-items
;;      (lambda () (dashboard-insert-newline 2))
;;      dashboard-insert-init-info
;;      (lambda () (dashboard-insert-newline 2))
;;      doom-dashboard-insert-homepage-footer))
;;   (dashboard-item-generators
;;    '((recents . doom-dashboard-insert-recents-shortmenu)
;;      (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
;;      (projects . doom-dashboard-insert-project-shortmenu)
;;      (agenda . doom-dashboard-insert-org-agenda-shortmenu)))
;;   :config
;;   (setq dashboard-startup-banner
;;         (concat doom-dashboard-banner-directory "isometric3.txt"))
;;   (dashboard-setup-startup-hook))

(provide 'appereance)
