;; https://github.com/syl20bnr/spacemacs/issues/12839
(setq package-check-signature nil)

;; load package manager, add the Melpa package registry
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

(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq gui-p         (display-graphic-p)
      cli-p         (not gui-p)
      android-p     (getenv "ANDROID_ROOT")
      linux-p       (and (eq system-type 'gnu/linux) (not android-p))
      windows-p     (eq system-type 'windows-nt)
      workstation-p (member (system-name)
                            '("berezhnev")))

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

(save-place-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(xterm-mouse-mode t)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(defun frostyx/set-default-font (size)
  (set-face-attribute
   'default nil
   ;; :family "Anonymous Pro" 
   :family "JetBrains Mono" 
   ;; :family "Fira Code"
   ;; :family "Iosevka"
   ;;:foundry "ADBO"
   :height size
   :weight 'light))

(frostyx/set-default-font 140)

;; (set-face-attribute 'default nil
;;  :family "Anonymous Pro"
;;  :height 150
;;  :weight 'light)
;; (set-face-attribute 'variable-pitch nil
;;  :family "JetBrains Mono"
;;  :height 1.0)
;; (set-face-attribute 'fixed-pitch nil
;;  :family "JetBrains Mono"
;;  :height 1.0)

(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(defun hook-tab-width ()
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq python-indent-offset 2))
(add-hook 'prog-mode-hook #'hook-tab-width)


;; Use latest Org
(use-package org
  :ensure t)

(use-package org-contrib
  :ensure t)

(server-start)

;; (set-frame-parameter (selected-frame) 'alpha '(85 . 80))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 80)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Tangle configuration
;;(org-babel-load-file (expand-file-name "berezhnev.org" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;; REGULAR ;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/regular.el")

;;;;;;;;;;;;;;;;;;;;;; EXWM ;;;;;;;;;;;;;;;;;;;;;
;; (load-file "~/.emacs.d/exwm.el")

;;;;;;;;;;;;;;;;;;;;;; EVIL ;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/evil.el")

;;;;;;;;;;;;;;;;;;;;;; ORG-MODE ;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/org-mode.el")

;;;;;;;;;;;;;;;;;;;;;; LANGUAGES ;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/languages.el")

;;;;;;;;;;;;;;;;;;;;;; DIRED ;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/dired.el")

;;;;;;;;;;;;;;;;;;;;;; AI ;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/ai.el")

;;;;;;;;;;;;;;;;;;;;;; GIT ;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/git.el")

;;;;;;;;;;;;;;;;;;;;;; MISC ;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lisp/misc.el")

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
