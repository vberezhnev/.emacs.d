; Disable package loading at startup (already present, kept for clarity)
(setq package-enable-at-startup nil)

;; Optimize garbage collection and process output (unchanged)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(require 'package)
(setq package-archives
      '(("myelpa" . "~/.myelpa/")
	("melpa" . "http://1.15.88.122/melpa/packages/")
	("gnu"   . "http://1.15.88.122/gnu/")
	("nongnu" . "http://1.15.88.122/gnu/nongnu/")))

;; Straight.el bootstrap (unchanged, as it’s required early)
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

(straight-use-package 'org)
(setq straight-use-package-by-default t)
(require 'use-package)

;; Initialize package.el (unchanged, but moved before use-package bootstrap)
(package-initialize)

;; Bootstrap use-package (optimized to avoid redundant installation)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Don’t auto-install unless :straight t
(setq use-package-always-defer nil)   ;; Default to deferring all packages


;; Other settings (unchanged, but included for context)
(server-start)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(electric-pair-mode)
(global-auto-revert-mode t)
(define-coding-system-alias 'UTF-8 'utf-8)

;; Custom file and other settings (unchanged)
(setq custom-file (concat user-emacs-directory "custom.el")
      mouse-yank-at-point t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
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

(defun my-delete-backward-word (arg)
  "Удаляет слово назад без добавления в kill-ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<backspace>") 'my-delete-backward-word)
(global-set-key (kbd "M-<backspace>") 'my-delete-backward-word)

;; Font settings (unchanged)
(set-face-attribute
 'default nil
 :family "Aporetic Sans Mono" ;; Iosevka
 :height 150)

;; System-packages: Load only when commands are invoked
(use-package system-packages
  :straight t
  :commands (system-packages-install system-packages-update system-packages-remove)
  :defer t)

;; Elpa-mirror: Configure only when package.el is used
(use-package elpa-mirror
  :straight t)

;; All-the-icons: Load only in graphical mode
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :defer t) ;; No commands needed, loaded by doom-modeline or other icon users

;; Nerd-icons: Load on demand
(use-package nerd-icons
  :straight t
  :defer t) ;; No commands needed, loaded by modes that use icons

;; Projectile: Load when keymap or commands are used
(use-package projectile
  :straight t
  :diminish projectile-mode
  :commands (projectile-mode projectile-find-file projectile-switch-project)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Templates2"))
  :config
  (projectile-mode +1))

;; Ripgrep: Load on demand for search commands
(use-package ripgrep
  :straight t
  :commands (ripgrep-regexp)
  :defer t)

;; Which-key: Load when activated or key bindings are used
(use-package which-key
  :straight t
  :commands (which-key-mode)
  :bind (("C-h C-k" . which-key-show-keymap))
  :init
  (which-key-mode +1))

;; Auto-package-update: Load only during update or at scheduled time
(use-package auto-package-update
  :straight t
  :defer 10 ;; Load after 10 seconds to allow scheduling
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 365
        auto-package-update-prompt-before-update nil
        auto-package-update-hide-results t)
  (auto-package-update-at-time "04:00"))

;; Vterm: Load only when vterm is invoked
(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :defer t)

;; Multi-vterm: Load when keybinding is used
(use-package multi-vterm
  :straight t
  :bind ("C-x e" . multi-vterm)
  :commands (multi-vterm)
  :defer t)

;; Bluetooth: Load on demand for bluetooth commands
(use-package bluetooth
  :straight t
  :commands (bluetooth-list-devices bluetooth-connect)
  :defer t
  :config
  (setq bluetooth-battery-warning-level 15))

;; Leetcode: Load when leetcode commands are used
(use-package leetcode
  :straight t
  ;; :commands (leetcode leetcode-try leetcode-submit)
  ;; :defer t
  :config
  (setq leetcode-prefer-language "rust"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/leetcode"))

;; Whisper: Load when whisper commands are used
(use-package whisper
  :straight (:host github :repo "natrys/whisper.el" :branch "master")
  :commands (whisper-run whisper-transcribe)
  :defer t
  :config
  (setq whisper-install-directory "/tmp/"
        whisper-model "large-v3-turbo"
        whisper-quantize "q5_0"
        whisper-language "ru"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)))

;; Whisper hook (unchanged)
(defun whisper--break-sentences (n)
  "Put a paragraph break every N sentences."
  (catch 'return
    (while t
      (dotimes (_ n)
        (forward-sentence 1)
        (when (eobp) (throw 'return nil)))
      (insert "\n\n")
      (when (= (char-after) ?\ )
        (delete-horizontal-space)))))

(add-hook 'whisper-post-process-hook
          (lambda ()
            (whisper--break-sentences 8)))

;; TRAMP and mode-line settings (unchanged)
(setq tramp-ssh-controlmaster-options "")
(setq tramp-verbose 10)

;; Olivetti: Load for specific modes
(use-package olivetti
  :straight (:host github :repo "rnkn/olivetti" :branch "master")
  :config
  (add-hook 'prog-mode-hook #'olivetti-mode)
  (add-hook 'org-mode-hook #'olivetti-mode)
  (add-hook 'telega-root-mode-hook #'olivetti-mode)
  (add-hook 'telega-chat-mode-hook #'olivetti-mode)
  (add-hook 'eww-mode-hook #'olivetti-mode)
  (add-hook 'nov-mode-hook #'olivetti-mode)
  ;; (add-hook 'elfeed-show-mode-hook #'olivetti-mode)
  ;; (add-hook 'elfeed-search-mode-hook #'olivetti-mode)

  (add-hook 'chess-game-hooks (lambda () (setq olivetti-body-width 90)))
  (add-hook 'eww-mode-hook (lambda () (setq olivetti-body-width 80)))
  (add-hook 'eww-buffers-mode-hook (lambda () (setq olivetti-body-width 80)))
  (add-hook 'nov-mode-hook (lambda () (setq olivetti-body-width 90)))

  (add-hook 'prog-mode-hook (lambda () (setq olivetti-body-width 130)))
  (add-hook 'org-mode-hook (lambda () (setq olivetti-body-width 130)))

  ;; (add-hook 'elfeed-search-mode-hook (lambda () (setq olivetti-body-width 90)))
  ;; (add-hook 'elfeed-show-mode-hook (lambda () (setq olivetti-body-width 90)))
  (add-hook 'telega-chat-mode-hook (lambda () (setq olivetti-body-width 110)))
  (add-hook 'telega-root-mode-hook (lambda () (setq olivetti-body-width 110))))

;; Theme-changer: Load for theme switching
(use-package theme-changer
  :straight (:host github :repo "hadronzoo/theme-changer" :branch "master")
  ;; :commands (change-theme)
  :demand t
  :init
  (setq calendar-location-name "Vladivostok, RU"
        calendar-latitude 43.11
        calendar-longitude 131.88)
  :config
  ;; (load-file "~/.emacs.d/lisp/themes/twitch-dark-theme.el") ;; Load theme lazily
  (change-theme 'leuven 'modus-vivendi)) ;; dichromacy | cobrakai

(use-package beacon
  :straight t)

(use-package chess
  :straight t)

(use-package alert
  :straight t) ;; dep for telega-alert

(use-package telega
  :straight (:host github :repo "zevlg/telega.el" :branch "master")
  :config

  (add-hook 'after-init-hook 'telega-notifications-mode)

  (setq telega-use-docker t)
  (add-hook 'telega-root-mode-hook
            (lambda ()
              (hl-line-mode 1)  ;; Включить hl-line-mode
              (evil-local-mode -1)  ;; Отключить evil-mode
              (telega-view-two-lines)))  ;; Двухстрочный вид
  (add-hook 'telega-chat-mode-hook
            (lambda ()
	      ;; (company-mode 1)
	      ;; (telega-company-emoji 1)
	      ;; (telega-company-username t)
              (hl-line-mode 1)  ;; Включить hl-line-mode
              (evil-local-mode -1)))  ;; Отключить evil-mode
  ;; Стилизация под Telegram Desktop (тёмная тема)
  (set-face-attribute 'telega-box-button nil
                      :background "#17212B"
                      :foreground "#E1E1E1"
                      :height 1.4
                      :box nil)
  (set-face-attribute 'telega-box-button-active nil
                      :background "#2B3744"
                      :foreground "#FFFFFF"
                      :height 1.4
                      :box nil)
  (set-face-attribute 'telega-root-heading nil
                      :background "#17212B"
                      :foreground "#4B8EFF"
                      :height 1.4
                      ;; :weight bold
		      )
  ;; (set-face-attribute 'default nil
  ;;                     :background "#17212B"
  ;;                     :foreground "#E1E1E1")
  ;; (set-face-attribute 'hl-line nil
  ;;                     :background "#2B3744"
  ;;                     :foreground nil
  ;;                     :inherit nil)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'prepend)
  ;; Кастомное лицо для иконок
  (defface telega-icon
    '((t :family "Noto Color Emoji" :height 1.6 :foreground "#E1E1E1"))
    "Face for icons in telega root buffer.")
  (defun telega-custom-icon-inserter (icon)
    "Insert ICON with telega-icon face."
    (propertize icon 'face 'telega-icon))
  (setq telega-folder-icons-alist
        `(("Channels" . ,(telega-custom-icon-inserter "🔊"))
          ("Groups" . ,(telega-custom-icon-inserter "👪"))
          ("Private" . ,(telega-custom-icon-inserter "🙋"))
          ("Main" . ,(telega-custom-icon-inserter "📢"))))
  (setq telega-root-fill-column 110)
  (setq telega-root-auto-fill-mode nil)
  (setq face-font-rescale-alist nil)
  (setq telega-auto-download-mode t)
  (setq telega-emoji-use-images nil)
  ;; (add-hook 'telega-load-hook 'global-telega-url-shorted-)

  (with-eval-after-load 'telega
    (load-file "~/.emacs.d/lisp/packages/telega/telega-alert.el")
    (load-file "~/.emacs.d/lisp/packages/telega/telega-emacs-stories.el")
    (load-file "~/.emacs.d/lisp/packages/telega/telega-url-shorten.el"))
  (require 'telega-emacs-stories)
  (require 'telega-alert)
  (telega-emacs-stories-mode 1)
  (define-key telega-root-mode-map (kbd "v e") 'telega-view-emacs-stories)
  (telega-alert-mode 1))

(use-package ace-link
  :config
  (ace-link-setup-default)
  ;; Bind ace-link for various modes in Evil normal state
  (with-eval-after-load 'evil
    ;; eww-mode
    (evil-define-key 'normal eww-mode-map
      (kbd "SPC f f") 'ace-link-eww)
    ;; org-mode
    (evil-define-key 'normal org-mode-map
      (kbd "SPC f f") 'ace-link-org)
    ;; info-mode
    (evil-define-key 'normal Info-mode-map
      (kbd "SPC f f") 'ace-link-info)
    ;; help-mode
    (evil-define-key 'normal help-mode-map
      (kbd "SPC f f") 'ace-link-help)
    ;; custom-mode (e.g., Customize interface)
    (evil-define-key 'normal Custom-mode-map
      (kbd "SPC f f") 'ace-link-custom)))

(use-package paredit
  :straight t)

(use-package annotate
  :straight t
  :hook (prog-mode . annotate-mode))

(setq tab-width 2)
(setq indent-tabs-mode t)

(use-package htmlize
  :straight t)

;; Beautify Org Checkbox Symbol
(defun my/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(add-hook 'org-mode-hook 'my/org-buffer-setup)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(use-package wttrin
  :straight t
  :config
  (setq wttrin-default-cities '("Tianjin" "Vladivostok")
	wttrin-default-locations '("Tianjin" "Vladivostok")
	wttrin-default-accept-language '("Accept-Language" . "ru-RU")))

;; (use-package display-wttr
;;   :straight t
;;   :config
;;   (setq display-wttr-locations '("Vladivostok")
;;         display-wttr-format "%c:+%t"))

(defun set-org-timer-to-2340 ()
  "Calculate time until 23:40:00 and set org-timer in HH:MM:SS format."
  (interactive)
  (require 'org-timer)
  (let* ((current-time (decode-time (current-time)))
         (current-hour (nth 2 current-time))
         (current-minute (nth 1 current-time))
         (current-second (nth 0 current-time))
         (current-total-seconds (+ (* current-hour 3600) (* current-minute 60) current-second))
         (target-hour 23)
         (target-minute 40)
         (target-second 0)
         (target-total-seconds (+ (* target-hour 3600) (* target-minute 60) target-second))
         (seconds-to-target (- target-total-seconds current-total-seconds)))
    ;; If current time is past 23:40, add 24 hours
    (when (< seconds-to-target 0)
      (setq seconds-to-target (+ seconds-to-target (* 24 3600))))
    ;; Debug output to check calculation
    (message "Current time: %02d:%02d:%02d, Seconds to 23:40:00: %d (%02d:%02d:%02d)"
             current-hour current-minute current-second
             seconds-to-target
             (/ seconds-to-target 3600)
             (/ (% seconds-to-target 3600) 60)
             (% seconds-to-target 60))
    ;; Format time as HH:MM:SS
    (let ((timer-string (format "%02d:%02d:%02d"
                                (/ seconds-to-target 3600)
                                (/ (% seconds-to-target 3600) 60)
                                (% seconds-to-target 60))))
      (org-timer-set-timer timer-string))))

;; Add to Emacs startup
(add-hook 'emacs-startup-hook 'set-org-timer-to-2340)

;; (use-package minimal-dashboard
;;   :straight (:host github :repo "dheerajshenoy/minimal-dashboard.el" :branch "main")
;;   :custom
;;   (minimal-dashboard-buffer-name "*dashboard*")
;;   (minimal-dashboard-image-path "~/.emacs.d/images/Emacs-logo.svg")
;;   (minimal-dashboard-text "хачумба")
;;   (minimal-dashboard-enable-resize-handling t)
;;   (minimal-dashboard-modeline-shown nil))
;; ;; (minimal-dashboard)
;; (setopt initial-buffer-choice #'minimal-dashboard)

(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  
  (use-package treemacs-evil
    :after (treemacs evil)
    :straight t)

  (use-package treemacs-projectile
    :after (treemacs projectile)
    :straight t)

  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :straight t)

  (use-package treemacs-magit
    :after (treemacs magit)
    :straight t))

(use-package writeroom-mode
  :straight t)

(use-package notmuch
  :straight t)

(use-package emms
  :straight t)

(use-package zoom
  :straight t
  :config
  (zoom-mode 1)
  (setq zoom-size '(0.618 . 0.618)))
(global-set-key (kbd "C-x +") 'zoom)

;; Load external files (unchanged, but ensure they respect lazy-loading)
(load-file "~/.emacs.d/lisp/evil.el")
(load-file "~/.emacs.d/lisp/appereance.el")

(load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/dired.el")
(load-file "~/.emacs.d/lisp/org/org.el")

(load-file "~/.emacs.d/lisp/eww.el")
(load-file "~/.emacs.d/lisp/readers.el")
(load-file "~/.emacs.d/lisp/rss.el")

(load-file "~/.emacs.d/lisp/company.el")
(load-file "~/.emacs.d/lisp/completion.el")
(load-file "~/.emacs.d/lisp/lsp.el")

(load-file "~/.emacs.d/lisp/enlight.el")

(provide 'init)
;; init.el ends here
