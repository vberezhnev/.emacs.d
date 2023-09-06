(require 'package)
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-quickstart nil)
;; ("melpa-stable" . "https://stable.melpa.org/packages/")
;; ("org" . "https://orgmode.org/elpa/")
;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))

(setq package-archive-priorities
      '(("melpa" .  3)
        ("nongnu" . 2)
        ("gnu" . 1)))

;; Configure Package Manager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

;; Set use-package-verbose to t for interpreted .emacs, and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;;________________________________________________________________
;;    Install use-package if not installed & configure
;;________________________________________________________________
;; (eval-and-compile
;; (unless (and (fboundp 'package-installed-p)
;;              (package-installed-p 'use-package))
;;   (package-refresh-contents) ; update archives
;;   (package-install 'use-package)) ; grab the newest use-package
;; (if init-file-debug
;;     (setq use-package-compute-statistics t)
;;   (setq use-package-compute-statistics nil))
;; (require 'use-package))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)  ; :ensure t  by default
  (use-package-always-defer  t) ; :defer t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

;;________________________________________________________________
;;    Install straight.el
;;________________________________________________________________
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;;________________________________________________________________
;;    Install quelpa
;;________________________________________________________________
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))
;; (require 'quelpa)

;;________________________________________________________________
;;    Install quelpa-use-package
;;________________________________________________________________
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)


;;________________________________________________________________
;;    Setup config using org-mode
;;________________________________________________________________
;; (org-babel-load-file
;;  (expand-file-name
;;   "README.org"
;;   user-emacs-directory))

;; (setq default-frame-alist '((undecorated . nil)))

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________

(server-start)

(set-frame-parameter (selected-frame) 'alpha '(97 .97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;;________________________________________________________________
;;    Base settings of Emacs
;;________________________________________________________________
(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		  ; Show battery.
(display-time-mode t)			  ; Show time.
(set-fringe-mode 1)               ; Give us some space.
(delete-selection-mode nil)		  ; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)     ; Set yes or no to y/n
(global-font-lock-mode 1)         ; always highlight code
(global-auto-revert-mode 1)       ; refresh a buffer if changed on disk
;; (global-hl-line-mode 1)           ; Highlight current line
(semantic-mode 1)								; help out with semantics
(savehist-mode 1)                 ; Save history
(save-place-mode 1)               ; when buffer is closed, save the cursor position
(blink-cursor-mode 1)

;; Setup fonts
;; (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 130)
;; (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Mono")
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 150)
(variable-pitch-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default shell-file-name "/bin/fish")

(setq ad-redefinition-action            'accept
      default-buffer-file-coding-system 'utf-8
      blink-cursor-interval             0.7       ;; Little slower cursor blinking . default is 0.5
      create-lockfiles                  nil
      idle-update-delay                 1.2    ;; Speed things up by not updating so often
      read-process-output-max           (* 8 1024 1024)
      ediff-split-window-function       'split-window-horizontally
      highlight-nonselected-windows     t
      auto-mode-case-fold               nil
      ;; backup-by-copying                 t
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      fast-but-imprecise-scrolling      t
      jit-lock-defer-time               0.0
      echo-keystrokes                   0.2
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      line-number-mode                  nil
      use-dialog-box                    nil
      load-prefer-newer                 t
      word-wrap                         nil
      visible-bell                      nil
      bidi-display-reordering           nil
      large-file-warning-threshold nil      ;; Disable "File is large. Really open?"
      x-stretch-cursor                  t   ;; stretch cursor on tabs
      scroll-margin                     4   ;; scroll N to screen edge
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.

(use-package go-mode)
(use-package pbcopy)

;; Disable backup
(setq backup-inhibited t)
;; Disable auto save
(setq auto-save-default nil)

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(add-hook 'minibuffer-exit-hook
	        '(lambda ()
	           (let ((buffer "*Completions*"))
	             (and (get-buffer buffer)
		                (kill-buffer buffer)))))
(setq initial-major-mode (quote fundamental-mode))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(global-display-line-numbers-mode t)

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
;;________________________________________________________________
;;    Setup fonts
;;________________________________________________________________
(set-face-attribute 'default t
                    :font "Iosevka" ;; Terminess Nerd Font Propo, Input, Terminess Nerd Font Propo
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

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Iosevka 12"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka 12"))

(add-to-list 'default-frame-alist '(font . "Iosevka 12"))
;; Changes certain keywords to symbols, such as lamda
(setq global-prettify-symbols-mode t)

;; Change color of divider (horizontal rules)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords
;;              nil
;;              '(("^-\\{5,\\}"  0 '(:foreground "green" :weight bold))))))

;;________________________________________________________________
;;    Setup theme
;;________________________________________________________________
(use-package gruvbox-theme)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package theme-changer
  :ensure t
  :demand t
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88))

(require 'theme-changer)
(change-theme 'doom-one-light 'doom-one)
;;________________________________________________________________
;;    Setup org-mode
;;________________________________________________________________
(use-package org
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq
   org-ellipsis " ▾" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
   org-roam-v2-ack t                 ; anonying startup message
   org-log-done 'time                ; I need to know when a task is done
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-log-into-drawer t
   org-log-done 'time
   org-startup-folded t
   ;; org-odd-levels-only t
   org-pretty-entities t
   org-startup-indented t
   org-adapt-indentation t
   org-hide-leading-stars t
   org-hide-macro-markers t
   org-hide-block-startup nil
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-hide-emphasis-markers t
   org-cycle-separator-lines 2
   org-startup-folded 'content
   org-startup-with-inline-images t
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 2
   org-fontify-quote-and-verse-blocks t
   org-export-with-smart-quotes t
   org-image-actual-width '(300))
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (require 'org-tempo)
    ;; Setup fonts for org-mode
    (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
    (add-hook 'org-babel-after-execute-hook (lambda ()
                                              (when org-inline-image-overlays
                                                (org-redisplay-inline-images))))
    (add-to-list 'org-modules 'org-tempo t))
  (setq org-display-remote-inline-images t)
  (add-to-list 'org-emphasis-alist
               '("[" (:foreground "red")))
;;; replace-org-char
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
						              '(("^ *\\([-]\\) "
						                 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Replace list plus with arrow
  (font-lock-add-keywords 'org-mode
						              '(("^ *\\([+]\\) "
						                 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "➤"))))))
  (use-package org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq
     ;; Edit settings
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     ;; org-insert-heading-respect-content t
     ;; Appearance
     ;; org-hide-leading-stars t
     ;; org-startup-indented nil
     org-modern-radio-target    '("❰" t "❱")
     org-modern-internal-target '("↪ " t "")
     org-modern-todo t
     org-modern-tag t
     org-modern-timestamp t
     org-modern-statistics t
     org-modern-table nil
     ;; org-modern-progress t Gives an error
     org-modern-priority t
     org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
     org-modern-hide-stars " "
     org-modern-keyword "‣"))
  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t
          org-appear-autolinks 'just-brackets))

  (use-package russian-holidays
    :config
    (require 'calendar)
    (require 'russian-holidays)
    (setq calendar-holidays russian-holidays))

  ;; Spacing of headings
  ;; (set-face-attribute 'org-document-title nil) ;; :font "Terminess Nerd Font Propo" :weight 'bold :height 1.5
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;;   (set-face-attribute (car face) nil)) ;;  :font "Terminess Nerd Font Propo" :weight 'medium :height (cdr face)
  ;; (use-package org-padding
  ;;   :straight (:host github :repo "TonCherAmi/org-padding")
  ;;   :config
  ;;   (setq org-padding-block-begin-line-padding '(2.0 . nil))
  ;;   (setq org-padding-block-end-line-padding '(nil . 1.0))
  ;;   (setq org-padding-heading-padding-alist
  ;;         '((4.0 . 1.5) (3.0 . 0.5) (3.0 . 0.5) (3.0 . 0.5) (2.5 . 0.5) (2.0 . 0.5) (1.5 . 0.5) (0.5 . 0.5))))
  (with-eval-after-load 'org
    (setq org-log-done 'time))
  ;; (setq org-todo-keywords
  ;;       '((sequence
  ;;          "TODO(t)"  ; A task that needs doing & is ready to do
  ;;          "PROJ(p)"  ; A project, which usually contains other tasks
  ;;          "LOOP(r)"  ; A recurring task
  ;;          "STRT(s)"  ; A task that is in progress
  ;;          "WAIT(w)"  ; Something external is holding up this task
  ;;          "HOLD(h)"  ; This task is paused/on hold because of me
  ;;          "IDEA(i)"  ; An unconfirmed and unapproved task or notion
  ;;          "|"
  ;;          "DONE(d)"  ; Task successfully completed
  ;;          "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
  ;;         (sequence
  ;;          "[ ](T)"   ; A task that needs doing
  ;;          "[-](S)"   ; Task is in progress
  ;;          "[?](W)"   ; Task is being held up or paused
  ;;          "|"
  ;;          "[X](D)")  ; Task was completed
  ;;         (sequence
  ;;          "|"
  ;;          "OKAY(o)"
  ;;          "YES(y)"
  ;;          "NO(n)"))
  ;;       org-todo-keyword-faces
  ;;       '(("[-]"  . +org-todo-active)
  ;;         ("STRT" . +org-todo-active)
  ;;         ("[?]"  . +org-todo-onhold)
  ;;         ("WAIT" . +org-todo-onhold)
  ;;         ("HOLD" . +org-todo-onhold)
  ;;         ("PROJ" . +org-todo-project)
  ;;         ("NO"   . +org-todo-cancel)
  ;;         ("KILL" . +org-todo-cancel)))
  (setq org-todo-keyword-faces
        '(
          ("TODO" :background "brown1" :foreground "black" :weight bold)
          ("DOING" :background "salmon" :foreground "black" :weight bold)
          ("NEXT" :background "sky blue" :foreground "black" :weight bold)
          ("DONE" :background "green yellow" :weight bold)
          ("ARCHIVED" :background "light slate blue" :weight bold)
          ("BLOCKED" :background "red" :foreground "black" :weight bold)))
  (setq org-todo-keywords
        '((sequence "TODO" "DOING" "NEXT" "|" "DONE" "ARCHIVED" "BLOCKED")))
  ;; (setq org-todo-keyword-faces
  ;;       '(("TODO" . (:foreground "#ff39a3" :weight bold))
  ;;         ("DOING" . "red")
  ;;         ("NEXT" . "blue")
  ;;         ("BLOCKED" . (:foreground "white" :background "#4d4d4d" :weight bold))))
  ;; ("POSTPONED" . "#008080"))

  (setq org-clock-sound "~/.emacs.d/sounds/sound.wav")
  (use-package org-alert)
  (use-package ob-typescript)
  (use-package ob-rust)
  (use-package ob-sql-mode)
  ;; Execute org src block
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (typescript . t)
     (shell . t)
     (python . t)
     (rust . t)
     (C . t)
     (sql . t)
     (latex . t)))
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (push '("#+begin_src rust" . "🦀" ) prettify-symbols-alist)
                             (push '("#+begin_quote" . "❝" ) prettify-symbols-alist)
                             (push '("#+end_quote" . "❞" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")

  (use-package org-cliplink
    :demand t)
  ;; (use-package focus
  ;;   :demand t
  ;;   :config
  ;;   '((prog-mode . defun) (text-mode . sentence)))
  (use-package org-recur
    :hook ((org-mode . org-recur-mode)
           (org-agenda-mode . org-recur-agenda-mode))
    :config
    (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)
    ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
    (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
    (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)
    (setq org-recur-finish-done t
          org-recur-finish-archive t))
  (use-package org-rainbow-tags
    :ensure t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;; ;; ORG ROAM SETTING ;; ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("a" "Atomic note (with source)" plain (file "~/Org/Templates/Atomic note.org")
      :if-new
      (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org" "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("t" "Article" plain (file "~/Org/Templates/Article.org")
      :if-new
      (file+head "articles/%<%Y-%m-%d-%H:%M>-article--${slug}.org" "#+title: ${title}\n#+filetags: :Article:\n#+date: %U\n\n")
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
     ("b" "Book notes" plain (file "~/Org/Templates/Book.org")
      :if-new (file+head "%<%Y-%m-%d-%H:%M>--book-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :Book:%^{Book type}:\n\n")
      :unnarrowed t)
     ("r" "Bibliography reference" plain (file "~/Org/Templates/Bibliography reference.org") ; <-- template store in a separate file
      :target
      (file+head "bibliography/references/${citekey}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :Book:%^{Book type}:")
      :unnarrowed t)))

  (org-roam-dailies-capture-templates
   '(
     ("m" "Morning diary" entry "* Morning (%U): %?\n\n** Что я сегодня обязан сделать?\n\n** Каковы мои ожидания от этого дня?" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %U\n\n" ))
     ("d" "Default diary" entry "* %U: «%?»‎\n\n" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %U\n\n" ))
     ("e" "Evening diary" entry "* Evening (%U): %?\n\n** Что я сделал за сегодня?\n\n** 3 вещи, за которые я благодарен?" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %U\n\n" ))))

  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))

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
  ;; :bind-keymap
  ;; ("C-c n d" . org-roam-dailies-map)
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  ;; Customize the org-roam buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-ui
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq orui-sync-theme nil
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil ))

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

;; Better sort exp.
(use-package org-transclusion
  :after org
  :config
  (define-key global-map (kbd "<f12>") #'org-transclusion-add)
  (define-key global-map (kbd "C-n t") #'org-transclusion-mode))

(use-package org-roam-timestamps
  :after org-roam
  :demand t
  :config (org-roam-timestamps-mode))
(setq org-roam-timestamps-parent-file t)
(setq org-roam-timestamps-remember-timestamps t)

(use-package org-download
  :demand t
  :config
  (setq-default org-download-image-dir "./assets-org/"))

;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)
;; (use-package org-noter)
;; (use-package org-noter
;;              :straight
;;              (:repo "org-noter/org-noter"
;;                     :host github
;;                     :type git
;;                     :files ("*.el" "modules/*.el")))

;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;; ;;
;; ;; CITES ;; ;;
;; ;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;

;; BibLaTeX settings
(setq bibtex-dialect 'biblatex)
(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq orb-note-actions-interface 'hydra) ;

  (setq orb-pdf-scrapper-export-options
        '((org  ;; <= TYPE
           ;;  Export to a heading in the buffer of origin
           (heading "References (extracted by ORB PDF Scrapper)"
                    ;; ^             ^
                    ;; TARGET     LOCATION
                    ;; PROPERTIES
                    ;;    v
                    :property-drawer ("PDF_SCRAPPER_TYPE"
                                      "PDF_SCRAPPER_SOURCE"
                                      "PDF_SCRAPPER_DATE")))
          (txt
           ;; Export to a file "references.org"
           (path "references.org"
                 ;; under a heading "New references"
                 :placement
                 (heading "New references"
                          :property-drawer ("PDF_SCRAPPER_TYPE"
                                            "PDF_SCRAPPER_SOURCE"
                                            "PDF_SCRAPPER_DATE")
                          ;; Put the new heading in front of other headings
                          :placement prepend)))
          (bib
           ;; Export to a file in an existing directory.  The file name will be CITEKEY.bib
           (path "/path/to/references-dir/"
                 :placement prepend
                 ;; Include only the references that are not in the target file
                 ;; *and* the file(s) specified in bibtex-completion-bibliography
                 :filter-bib-entries bibtex-completion-bibliography))))

  (setq orb-autokey-format "%A*%y")) ;; => "DoeJohn2020"

(use-package org-ref
  :config
  (setq bibtex-completion-bibliography '(
					                               "~/Nextcloud/Org/Org-roam/bibliography/master.bib"
                                         ;; "~/Nextcloud/Org/Org-roam/bibliography/references.bib"
					                               ;; "~/Nextcloud/Org/Org-roam/bibliography/dei.bib"
					                               ;; "~/Nextcloud/Org/Org-roam/bibliography/archive.bib"
                                         )
	      bibtex-completion-library-path '("~/Nextcloud/Org/Org-roam/bibliography/pdfs/")
	      bibtex-completion-notes-path "~/Nextcloud/Org/Org-roam/bibliography/notes/"
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
	        (call-process "open" nil 0 nil fpath))
        )

  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

  (require 'org-ref)
  (require 'org-ref-helm)

  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "H-c") org-ref-insert-cite-function)
  (define-key org-mode-map (kbd "H-r") org-ref-insert-ref-function)
  (define-key org-mode-map (kbd "H-l") org-ref-insert-label-function)
                                        ;
  ;; LaTeX/PDF export
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package helm-bibtex
  :config

  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (setq bibtex-completion-bibliography
				"~/Nextcloud/Org/Org-roam/bibliography/master.bib"
        ;; '("~/Nextcloud/Org/Org-roam/bibliography/references.bib"
				;; 	"~/Nextcloud/Org/Org-roam/bibliography/dei.bib"
				;; "~/Nextcloud/Org/Org-roam/bibliography/archive.bib")
        )
  ;; PDFs
  (setq bibtex-completion-library-path '("~/Nextcloud/Org/Org-roam/bibliography/pdfs"))
  (setq bibtex-completion-pdf-field "File")

  ;; Notes
  (setq bibtex-completion-notes-path "~/Nextcloud/Org/Org-roam/bibliography/notes")

  (setq org-cite-follow-processor 'helm-bibtex-org-cite-follow)

  (setq bibtex-completion-pdf-symbol "📁")
  (setq bibtex-completion-notes-symbol "✎"))

(use-package citar
  :custom
  (citar-bibliography '(
		                    "~/Nextcloud/Org/Org-roam/bibliography/master.bib"
                        ;; "~/Nextcloud/Org/Org-roam/bibliography/references.bib"
		                    ;; "~/Nextcloud/Org/Org-roam/bibliography/dei.bib"
		                    ;; "~/Nextcloud/Org/Org-roam/bibliography/archive.bib")
                        )
                      :config
                      (setq citar-templates
                            '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
                              (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                              (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                              (note . "Notes on ${author editor:%etal}, ${title}")))
                      (setq citar-indicators
                            (list citar-indicator-files ; plain text
                                  citar-indicator-notes-icons)) ; icon
                      :hook
                      (LaTeX-mode . citar-capf-setup)
                      (org-mode . citar-capf-setup)))

(use-package citar-embark)
(use-package company-bibtex)
(use-package citar-org-roam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; ORG PDF, ORG NOTER ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))
(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
    With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-agenda
  :ensure nil
  :demand t
  :defer t
  :config
  (use-package org-super-agenda
    :demand t)

  ;; (setq org-agenda-files (list "~/Org/agenda/PlanAhead.org" "~/Org/agenda/PlannedDay.org")
  ;; org-log-done 'time)

  (setq org-agenda-files '("~/Org/agenda"))
  (setq org-cycle-separator-lines 2)
  (setq org-directory "~/Org")
  ;; (setq org-agenda-include-diary t) ;; Calendar/Diary integration
  ;; (setq org-default-notes-file "~/Org/agenda/notes.org")

  ;; (use-package org-agenda-property
  ;;   :config
  ;;   (customize-set-variable 'org-agenda-property-list
  ;;                           '("WAITING?" "DEFERRED?" "CANCELLED?" "NOTE"))
  ;;   (customize-set-variable 'org-agenda-property-position 'same-line))

  ;; (require 'find-lisp)
  ;; (defun who/find-org-files (directory)
  ;;   (find-lisp-find-files directory "\.org$"))

  ;; (defun who-org/agenda-files-update (&rest _)
  ;;   (let ((todo-zettels (->> "rg --files-with-matches '(TODO)|(NEXT)|(DONE)|(REPEAT)|(DONE)|(HOLD)|(PROJ)|(WAIT)|(REVIEW)|(IDEA)|(STOP)|(EVENT)' ~/Org/Org-roam/"
  ;;                            (shell-command-to-string)
  ;;                            (s-lines)
  ;;                            (-filter (lambda (line) (not (s-blank? line)))))))
  ;;     (setq org-agenda-files (append (who/find-org-files who/org-agenda-directory)
  ;;                                    todo-zettels))))
  ;; (advice-add 'org-agenda :before #'who-org/agenda-files-update)

  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t)
  ;; (with-eval-after-load 'org-journal
  ;;   (define-key org-journal-mode-map (kbd "<C-tab>") 'yas-expand))
  (setq org-agenda-clockreport-parameter-plist
        (quote (:link t :maxlevel 5 :fileskip t :compact t :narrow 80)))
  (setq org-agenda-deadline-faces
        '((1.0001 . org-warning)              ; due yesterday or before
          (0.0    . org-upcoming-deadline)))  ; due today or later(setq-default org-icalendar-include-todo t)
  ;; (setq org-combined-agenda-icalendar-file "~/Org/calendar.ics")
  ;; (icalendar-import-file "~/Org/calendar.ics" "diary-google")
  (setq org-icalendar-combined-name "Hugo Org")
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  (setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
  (setq org-icalendar-timezone "Asia/Vladivostok")
  (setq org-icalendar-store-UID t)
  (setq org-icalendar-alarm-time 30)
  (setq calendar-date-style 'european
        calendar-mark-holidays-flag t
        calendar-week-start-day 1)
  ;; calendar-mark-diary-entries-flag t

  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (defun my/style-org-agenda()
    ;; (my/buffer-face-mode-variable)
    (set-face-attribute 'org-agenda-date nil :height 1.1)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                 "\n"
                                                 (org-agenda-format-date-aligned date))))

  (setq org-agenda-custom-commands
        '(("z" "Hugo view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(;; Each group has an implicit boolean OR operator between its selectors.
                            (:name "Today"
                                   :deadline today
                                   :face (:background "black"))
                            (:name "Passed deadline"
                                   :and (:deadline past :todo ("TODO" "DOING" "BLOCKED" "REVIEW"))
                                   :face (:background "#7f1b19"))
                            (:name "Work important"
                                   :and (:priority>= "B" :category "work" :todo ("TODO" "NEXT")))
                            (:name "Work other"
                                   :and (:category "work" :todo ("TODO" "NEXT")))
                            (:name "Important"
                                   :priority "A")
                            (:priority<= "B"
                                         ;; Show this section after "Today" and "Important", because
                                         ;; their order is unspecified, defaulting to 0. Sections
                                         ;; are displayed lowest-number-first.
                                         :order 1)
                            (:name "Papers"
                                   :file-path "org/roam/notes")
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 9)
                            (:name "On review"
                                   :todo "REVIEW"
                                   :order 10)))))))))
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(setq org-capture-templates
      '(("d" "Daily task" entry (file+function
                                 "~/Org/agenda/PlannedDay.org"
                                 (lambda ()
                                   (org-datetree-find-date-create
                                    (org-date-to-gregorian (org-today)) t)
                                   (re-search-forward "^\\*.+ log" nil t)))
         "* TODO something\n* TODO something")))

;; (use-package org-caldav
;;   :custom
;;   (org-caldav-url "https://lunarcloud.ddns.net/remote.php/dav/calendars/ncp")
;;   (org-caldav-calendar-id "personal")
;;   ;; (org-caldav-calendar-id "XZRo6AtbiyiaBbpE")
;;   (org-caldav-inbox "~/Org/agenda/calendar.org")
;;   ;; (org-caldav-files '("~/Org/agenda/calendar.org"))
;;   (org-icalendar-timezone "Asia/Vladivostok")
;;   (org-caldav-delete-org-entries 'never)
;;   (org-caldav-sync))

(use-package ox-hugo
  ;;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :config
  (use-package org-re-reveal  )
  (use-package ox-reveal  )
  (setq org-reveal-root "file:~/Org/Presentations/reveal.js/"))

;; Matrix
(use-package ement)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "ru_RU")
(setq ispell-local-dictionary-alist
      '(("ru_RU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package pdf-tools
  :defer t
  ;; :mode (("\\.pdf\\'" . pdf-view-mode))
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

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

(use-package cl-lib  )
(setq sql-sqlite-program "/usr/bin/sqlite3")
;; (setq calibredb-program "/Applications/calibre.app/Contents/MacOS/calibredb")

;; (use-package calibredb
;;   :defer t
;;   :config
;;   (setq calibredb-root-dir "~/Calibre Library")
;;   (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
;;   (setq calibredb-library-alist '(("~/Books")))
;;   (setq calibredb-virtual-library-alist '(("1. Development - work" . "work \\(pdf\\|epub\\)")
;;                                           ("2. Read it later" . "Readit epub")
;;                                           ("3. Development - rust" . "rust")))
;;   (setq calibredb-format-all-the-icons t)
;;   (setq calibredb-format-icons-in-terminal t))

;; ;; Keybindings
;; (defvar calibredb-show-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "?" #'calibredb-entry-dispatch)
;;     (define-key map "o" #'calibredb-find-file)
;;     (define-key map "O" #'calibredb-find-file-other-frame)
;;     (define-key map "V" #'calibredb-open-file-with-default-tool)
;;     (define-key map "s" #'calibredb-set-metadata-dispatch)
;;     (define-key map "e" #'calibredb-export-dispatch)
;;     (define-key map "q" #'calibredb-entry-quit)
;;     (define-key map "y" #'calibredb-yank-dispatch)
;;     (define-key map "," #'calibredb-quick-look)
;;     (define-key map "." #'calibredb-open-dired)
;;     (define-key map "\M-/" #'calibredb-rga)
;;     (define-key map "\M-t" #'calibredb-set-metadata--tags)
;;     (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
;;     (define-key map "\M-A" #'calibredb-set-metadata--authors)
;;     (define-key map "\M-T" #'calibredb-set-metadata--title)
;;     (define-key map "\M-c" #'calibredb-set-metadata--comments)
;;     map)
;;   "Keymap for `calibredb-show-mode'.")

;; (defvar calibredb-search-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mouse-3] #'calibredb-search-mouse)
;;     (define-key map (kbd "<RET>") #'calibredb-find-file)
;;     (define-key map "?" #'calibredb-dispatch)
;;     (define-key map "a" #'calibredb-add)
;;     (define-key map "A" #'calibredb-add-dir)
;;     (define-key map "c" #'calibredb-clone)
;;     (define-key map "d" #'calibredb-remove)
;;     (define-key map "D" #'calibredb-remove-marked-items)
;;     (define-key map "j" #'calibredb-next-entry)
;;     (define-key map "k" #'calibredb-previous-entry)
;;     (define-key map "l" #'calibredb-virtual-library-list)
;;     (define-key map "L" #'calibredb-library-list)
;;     (define-key map "n" #'calibredb-virtual-library-next)
;;     (define-key map "N" #'calibredb-library-next)
;;     (define-key map "p" #'calibredb-virtual-library-previous)
;;     (define-key map "P" #'calibredb-library-previous)
;;     (define-key map "s" #'calibredb-set-metadata-dispatch)
;;     (define-key map "S" #'calibredb-switch-library)
;;     (define-key map "o" #'calibredb-find-file)
;;     (define-key map "O" #'calibredb-find-file-other-frame)
;;     (define-key map "v" #'calibredb-view)
;;     (define-key map "V" #'calibredb-open-file-with-default-tool)
;;     (define-key map "," #'calibredb-quick-look)
;;     (define-key map "." #'calibredb-open-dired)
;;     (define-key map "y" #'calibredb-yank-dispatch)
;;     (define-key map "b" #'calibredb-catalog-bib-dispatch)
;;     (define-key map "e" #'calibredb-export-dispatch)
;;     (define-key map "r" #'calibredb-search-refresh-and-clear-filter)
;;     (define-key map "R" #'calibredb-search-clear-filter)
;;     (define-key map "q" #'calibredb-search-quit)
;;     (define-key map "m" #'calibredb-mark-and-forward)
;;     (define-key map "f" #'calibredb-toggle-favorite-at-point)
;;     (define-key map "x" #'calibredb-toggle-archive-at-point)
;;     (define-key map "h" #'calibredb-toggle-highlight-at-point)
;;     (define-key map "u" #'calibredb-unmark-and-forward)
;;     (define-key map "i" #'calibredb-edit-annotation)
;;     (define-key map (kbd "<DEL>") #'calibredb-unmark-and-backward)
;;     (define-key map (kbd "<backtab>") #'calibredb-toggle-view)
;;     (define-key map (kbd "TAB") #'calibredb-toggle-view-at-point)
;;     (define-key map "\M-n" #'calibredb-show-next-entry)
;;     (define-key map "\M-p" #'calibredb-show-previous-entry)
;;     (define-key map "/" #'calibredb-search-live-filter)
;;     (define-key map "\M-t" #'calibredb-set-metadata--tags)
;;     (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
;;     (define-key map "\M-A" #'calibredb-set-metadata--authors)
;;     (define-key map "\M-T" #'calibredb-set-metadata--title)
;;     (define-key map "\M-c" #'calibredb-set-metadata--comments)
;;     map)
;;   "Keymap for `calibredb-search-mode'.")

(use-package djvu)

(use-package dashboard
  ;; :straight (:build t)
  :ensure t
  :defer nil
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Welcome back, Darling!"
        dashboard-startup-banner "~/.emacs.d/images/Emacs-logo.svg"
        dashboard-center-content    t
        dashboard-show-shortcuts    nil
        dashboard-set-navigator     nil
        dashboard-set-heading-icons nil
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-set-file-icons    t)

  ;; (setq dashboard-navigator-buttons
  ;; 			`(;; line1
  ;; 				((,(all-the-icons-wicon "tornado" :height 1.1 :v-adjust 0.0)
  ;; 					"Main site"
  ;; 					"Browse homepage"
  ;; 					(lambda (&rest _) (browse-url "https://berezhnev.netlify.app"))
  ;; 				  ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error)))
  ;; 				;; line 2
  ;; 				((,(all-the-icons-faicon "github" :height 1.1 :v-adjust 0.0)
  ;; 					"Github"
  ;; 					""
  ;; 					(lambda (&rest _) (browse-url "https://github.com/tell396"))))))

  (setq dashboard-items '((recents  . 8)
                          (agenda   . 14)
                          (projects . 6)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

(use-package ligature
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

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :demand t
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-hide-details-mode nil)
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Use `all-the-icons' as Dirvish's icon backend
;; (use-package all-the-icons)

'(global-dired-hide-details-mode t)
(use-package dired
  :ensure nil
  :demand t
  :defer t
  :config
  (setq dired-dwim-target t) ; Dired tries to guess the target directory
  (setq dired-recursive-deletes 'always) ; Allow deleting directories recursively
  (setq dired-listing-switches "-alh --group-directories-first") ; Use human-readable file sizes and group directories first
  (setq dired-hide-details-mode t) ; Hide file and directory details by default
  (setq dired-auto-revert-buffer t) ; Automatically refresh Dired buffers when changes are made
  (setq diredp-hide-details-initially-flag nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; Bind Enter to open files
  (define-key dired-mode-map (kbd "^")
	  (lambda () (interactive) (find-alternate-file ".."))) ; Bind ^ to go up a directory
  (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode) ; Bind ( to toggle file and directory details
  (define-key dired-mode-map (kbd "N") 'dired-create-file) ; Bind N to create a new file
  (define-key dired-mode-map (kbd "n") 'dired-create-directory) ; Bind n to create a new directory
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)

  ;; (use-package all-the-icons-dired
	;;   :hook (dired-mode . all-the-icons-dired-mode) ; Display icons in Dired mode
	;;   :init
	;;   (setq all-the-icons-dired-mode-inline-electric-icons t))) ; Show electric icons for Dired mode

  (use-package dired-rainbow
    :config
    (progn
      (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
      (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
      (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
      (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
      (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
      (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
      (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
      (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
      (dired-rainbow-define log "#c17d11" ("log"))
      (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
      (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
      (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
      (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
      (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
      (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
      (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
      (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
      (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
      (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
      (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

  (use-package dired-open
    :config
    ;; Doesn't work as expected!
    (add-to-list 'dired-open-functions #'dired-open-xdg t))

  (use-package dired-sidebar
    :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
    :ensure t
    :commands (dired-sidebar-toggle-sidebar)
    :init
    (add-hook 'dired-sidebar-mode-hook
              (lambda ()
                (unless (file-remote-p default-directory)
                  (auto-revert-mode))))
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-subtree-line-prefix "__")
    (setq dired-sidebar-theme 'vscode)
    (setq dired-sidebar-use-term-integration t)
    (setq dired-sidebar-use-custom-font t))

  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" 'dired-single-up-directory
  ;;   "l" 'dired-single-buffer)
  (use-package dired-single))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-bar-width 5
        doom-modeline-buffer-file-name-style 'truncate-except-project
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
  (set-face-attribute 'region nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))
  (set-face-attribute 'highlight nil
                      :foreground (face-background 'doom-modeline-bar)
                      :background (face-background 'default))
  (eval-when-compile
    'company
    (doom-modeline-def-segment company-backend
      "Display the current company backend. `company-backend'."
      (when (company--active-p)
        (format "%s"
                (--map (s-replace "company-" "" (format "%s" it))
                       (if (listp company-backend) company-backend (list company-backend)))))))
  (doom-modeline-def-segment
    buffer-info
    "Overwrite of buffer info to not include the icon"
    (concat
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))
  (doom-modeline-def-segment
    buffer-type
    "Buffer icon and version if it exists"
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (when (and doom-modeline-env-version doom-modeline-env--version)
       (propertize
        (format "%s " doom-modeline-env--version)
        'face '(:height 0.7)))))
  (doom-modeline-def-modeline
    'main
    '(bar workspace-name window-number modals matches buffer-type buffer-info remote-host)
    '(company-backend misc-info persp-name battery debug lsp input-method buffer-encoding  process vcs checker)))

(use-package minions
  :delight " 𝛁"
  :hook (doom-modeline-mode . minions-mode)
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]"))

(use-package elfeed
  :config
  ;; data is stored in ~/.elfeed
  (setq elfeed-feeds
        '(
          ;; freelance
          ;;("https://freelance.habr.com/user_rss_tasks/vsE2OtRKoyNeUnK7RGd+0w==" freelance)
          ;;
          ("https://habr.com/ru/rss/feed/posts/all/bd769e8234cb6e6444ae3197fd0c0d9b/?fl=ru" habr-my-topics)
          ;; programming
          ;;("https://news.ycombinator.com/rss" hacker)
          ;;("https://www.reddit.com/r/programming.rss" programming)
          ;;("https://www.reddit.com/r/emacs.rss" emacs)
          ("https://www.opennet.ru/opennews/opennews_all_utf.rss" opennet-news)
          ;; ("https://habr.com/ru/rss/all/all/?fl=ru" habr-all)
          ;;("https://habr.com/ru/rss/news/?fl=ru" habr-news)
          ("https://nuancesprog.ru/feed" nop)
          ;;("https://dev.to/feed" dev-to)
          ;; hobby
          ("https://www.reddit.com/r/nasa.rss" nasa)
          ("https://habr.com/ru/rss/hub/astronomy/all/?fl=ru" habr-astronomy)
          ;; ("https://habr.com/ru/rss/flows/popsci/all/?fl=ru" habr-popsci)
          ("https://nplus1.ru/rss" np1)
          ;; programming languages
          ("https://www.reddit.com/r/javascript.rss" javascript)
          ("https://www.reddit.com/r/typescript.rss" typescript)
          ("https://www.reddit.com/r/golang.rss" golang)
          ("https://www.reddit.com/r/rust.rss" rust)
          ;; Books
          ;; ("https://habr.com/ru/rss/hub/read/all/?fl=ru" habr-books)
          ;; cloud
          ;;("https://www.reddit.com/r/aws.rss" aws)
          ;;("https://www.reddit.com/r/googlecloud.rss" googlecloud)
          ;;("https://www.reddit.com/r/azure.rss" azure)
          ;;("https://www.reddit.com/r/devops.rss" devops)
          ;;("https://www.reddit.com/r/kubernetes.rss" kubernetes)
          ))
  (setq-default elfeed-search-filter "@7-days-ago +unread")
  (setq-default elfeed-search-title-max-width 100)
  (setq-default elfeed-search-title-min-width 100))

(use-package evil
  ;; :defer nil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package general
  :after evil
  :config
  (general-evil-setup t))

(use-package evil-collection
  :defer nil
  :after evil
  :config
  (setq evil-emacs-state-cursor '("#FF5D62" box))
  (setq evil-normal-state-cursor '("#FF5D62" box))
  (setq evil-visual-state-cursor '("#98BB6C" box))
  (setq evil-insert-state-cursor '("#E82424" bar))
  (setq evil-replace-state-cursor '("#FF9E3B" hbar))
  (setq evil-operator-state-cursor '("#7E9CD8" hollow))
	(evil-set-initial-state 'ibuffer-mode 'normal)
	(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
	(evil-set-initial-state 'vterm-mode 'normal)
	(evil-set-initial-state 'calibredb-mode 'normal)
	;; (evil-set-initial-state 'dired-mode 'emacs)
	(evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll "
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))
(defun fzf-node-project ()
  (interactive)
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=ag -g \"\" --ignore node_modules .git build dist")
               process-environment)))
    (fzf/start default-directory)))

(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
		    magit-todos-depth 4
		    magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*" "node_modules/" "target/"))
  (custom-set-variable
   '(magit-todos-keywords (list "TODO" "FIXME" "BUGFIX" "HACK"))))

(use-package blamer
  ;; :bind (("s-i" . blamer-show-commit-info)
  ;;        ("C-c i" . ("s-i" . blamer-show-posframe-commit-info)))
  :defer nil
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (setq blamer-view 'overlay
        blamer-type 'posframe-popup
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
        blamer-author-formatter " ✎ [%s] - "
        blamer-commit-formatter "● %s ● ")
  (global-blamer-mode 1))

(defun blamer-callback-show-commit-diff (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (magit-show-commit commit-hash))))

(defun blamer-callback-open-remote (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (message commit-hash)
      (forge-browse-commit commit-hash))))

(setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                        ("<mouse-1>" . blamer-callback-show-commit-diff)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :defer nil
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :defer nil
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(defun my/highlight-todo-like-words ()
  (font-lock-add-keywords
   nil `(("\\<\\(FIXME\\|TODO\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'my/highlight-todo-like-words)
(setq projectile-globally-ignored-files "node_modules")

;;________________________________________________________________
;;    Telega.el
;;________________________________________________________________
(use-package telega
  :config
  (setq telega-use-docker t)
  (add-hook 'telega-load-hook 'telega-notifications-mode)
  (add-hook 'telega-load-hook 'telega-appindicator-mode)
  (add-hook 'telega-load-hook 'global-telega-url-shorten-mode))

;;________________________________________________________________
;;    Treemacs
;;________________________________________________________________
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

(use-package treemacs-evil
  :after (treemacs evil)
  :defer nil
  :demand t
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :demand t
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :demand t
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :demand t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :demand t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :init
  (treemacs-load-theme "doom-colors"))

(use-package lsp-treemacs
  :after treemacs
  :demand t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package vterm
  :demand t)

(use-package multi-vterm
  :ensure t
  :after vterm
  :bind
  ("C-x q" . vterm-clear)
  ("C-x w" . multi-vterm))

(use-package helm
  :init
  ;; Настройка сочетаний клавиш для Helm
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  :config
  ;; Оптимизация Helm для улучшения производительности
  (setq
   ;; helm-candidate-number-limit 500
   ;;      helm-idle-delay 0.0
   ;;      helm-input-idle-delay 0.01
   helm-quick-update t
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   ;; helm-recentf-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-completion-in-region-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-move-to-line-cycle-in-source t
   helm-scroll-amount 8
   helm-ff-file-name-history-use-recentf nil
   helm-echo-input-in-header-line nil))

;; Needed for `:after char-fold' to work
(use-package char-fold
  :demand t
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :ensure t ; install `reverse-im' using package.el
  :demand t ; always load it
  :after char-fold ; but only after `char-fold' is loaded
  :bind
  ("M-T" . reverse-im-translate-word) ; fix a word in wrong layout
  :custom
  (reverse-im-char-fold t) ; use lax matching
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer")) ; translate these methods
  :config
  (reverse-im-mode t)) ; turn the mode on

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") 'ian/format-code)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c e") #'emojify-insert-emoji)) ; override binding in any mode

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config (which-key-mode))

(use-package zygospore  )
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

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

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(defun my/parrot-animate-when-compile-success (buffer result)
  (if (string-match "^finished" result)
	    (parrot-start-animation)))

;; (use-package parrot
;;   :config
;;   (parrot-mode)
;;   (parrot-set-parrot-type 'thumbsup)
;;   (add-hook 'before-save-hook 'parrot-start-animation)
;;   (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode +1)
  :delight)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-auto-revert-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-x") 'helm-M-x)

;;Org
(global-set-key (kbd "M-q") #'toggle-truncate-lines)
;; Org agenda
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; Org timer
(global-set-key (kbd "C-c t s") #'org-timer-set-timer)
(global-set-key (kbd "C-c t SPC") #'org-timer-pause-or-continue)
(global-set-key (kbd "C-c t <deletechar>") #'org-timer-stop)

(global-set-key (kbd "\C-c w") 'evil-window-map)

(global-set-key (kbd "\C-c f") 'format-all-buffer)

(xterm-mouse-mode t)

(setq-default tab-width 2) ; set default tab char's display width to 2 spaces
(setq tab-width 2)         ; set current buffer's tab char's display width to 2 spaces

(dolist (mode '(org-mode-hook ; Disable line numbers for some modes
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

(helm-mode 1)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  ;; Оптимизация компании для ускорения загрузки
  (setq company-minimum-prefix-length 3
        ;; company-idle-delay 0.2
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case nil
        company-etags-ignore-case nil
        company-etags-file-name-prefix ""
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '((company-capf :with company-yasnippet)
                           company-files
                           (company-dabbrev-code company-gtags company-keywords)
                           company-dabbrev)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        ;; Move company-box-icons--elisp to the end, because it has a catch-all
        ;; clause that ruins icons from other backends in elisp buffers.
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions))
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))

  ;; HACK Fix oversized scrollbar in some odd cases
  ;; REVIEW `resize-mode' is deprecated and may stop working in the future.
  ;; TODO PR me upstream?
  (setq x-gtk-resize-child-frames 'resize-mode)
  ;; Disable tab-bar in company-box child frames
  ;; TODO PR me upstream!
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))
  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

(use-package company-auctex
	:after (latex)
  :config
  ;; Set up default LaTeX preview configuration
  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-preview-latex-default-process 'imagemagick) ; or 'dvisvgm
  (setq org-preview-latex-process-alist
        '((imagemagick :programs ("latex" "convert")
                       :description "imagemagick"
                       :message "You need to install the programs: latex and imagemagick."
                       :image-input-type "pdf"
                       :image-output-type "png"
                       :image-size-adjust (1.0 . 1.0)
                       :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
          (dvisvgm :programs ("latex" "dvisvgm")
                   :description "dvisvgm"
                   :message "You need to install the programs: latex and dvisvgm."
                   :image-input-type "xdv"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))
  ;; Enable inline LaTeX previews in org-mode
  (add-hook 'org-mode-hook 'org-toggle-latex-fragment)
  ;; Display images in org-mode buffers
  (setq org-image-actual-width nil) ; adjust to your liking
  (setq org-startup-with-inline-images t)
  (use-package ac-math))
(company-auctex-init)

(use-package company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package yasnippet  )

;;________________________________________________________________
;;;    LSP & Completion
;;________________________________________________________________

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-tex-server 'digestif)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         (javascript-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (rust-mode . lsp)
         (LaTeX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(add-hook 'js-mode-hook #'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp-mode) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-mode) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp-mode) ;; for rjsx-mode support

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; Symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

(use-package sideline
  :init
  (setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        sideline-display-backend-name t))      ; display the backend name

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package corfu
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)

;; optionally if you want to use debugger
(use-package dap-mode  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-ui  )

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :commands web-mode
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  )

(use-package import-js  )

;; JSX syntax highlighting
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(use-package js2-mode   :defer 20
  :mode
  (("\\.js\\'" . js2-mode))
  :custom
  (js2-include-node-externs t)
  ;;(js2-global-externs '("customElements"))
  (js2-highlight-level 3)
  (js2r-prefer-let-over-var t)
  (js2r-prefered-quote-type 2)
  (js-indent-align-list-continuation t)
  (global-auto-highlight-symbol-mode t)
  :config
  (setq js-indent-level 2)
  ;; patch in basic private field support
  (advice-add #'js2-identifier-start-p
              :after-until
              (lambda (c) (eq c ?#))))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

;; (add-hook 'web-mode-hook #'(lambda ()
;;                              (enable-minor-mode
;;                               '("\\.tsx?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook 'prettier-js-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package prettier-js)

(use-package tide
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq tide-format-options '(:tabSize 2 :indentSize 2 ))
;; TSX with treesitter
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)

(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(use-package json-mode   :defer 20
  :custom
  (json-reformat:indent-width 2)
  :mode (("\\.bowerrc$"     . json-mode)
		     ("\\.jshintrc$"    . json-mode)
		     ("\\.json_schema$" . json-mode)
		     ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
				          :map json-mode-map
				          ("C-c <tab>" . json-mode-beautify)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  (add-hook 'before-save-hook 'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'lsp-go-install-save-hooks)

;; (lsp-register-custom-settings
;; 		'(("gopls.completeUnimported" t t)
;; 		("gopls.staticcheck" t t)))

(use-package rust-playground  )

(use-package rust-mode

  :if (executable-find "rustc"))

(use-package cargo

  :if (executable-find "cargo")
  :after rust-mode
  :bind (:map cargo-minor-mode-map
              ("C-c C-t" . cargo-process-test)
              ("C-c C-b" . cargo-process-build)
              ("C-c C-c" . cargo-process-run))
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(add-hook 'rust-mode-hook 'lsp-deferred)

(use-package tree-sitter

  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))


(use-package tree-sitter-langs

  :after tree-sitter)

;;________________________________________________________________
;;;    Flycheck
;;________________________________________________________________

(use-package flycheck

  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.2)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 2)
	:config
	;; enable typescript-tslint checker
	(flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck-inline

  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust

  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia

  :config
  (apheleia-global-mode +1))

;; (add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold better-gc-cons-threshold)
;;             (setq file-name-handler-alist file-name-handler-alist-original)
;;             (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;;________________________________________________________________
;;;;    Fancy pkg
;;________________________________________________________________
(use-package fancy-battery
  :config
  (setq fancy-battery-show-percentage t)
  (setq battery-update-interval 15)
  (if window-system
      (fancy-battery-mode)
    (display-battery-mode)))

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
  (olivetti-body-width 120)
  :delight " ⊗") ; Ⓐ ⊛

;;;;; hl-indent
(use-package highlight-indent-guides
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
;; (set-face-background 'highlight "#3e4446") ; also try: "#3e4446"/"#gray6"
;; (set-face-foreground 'highlight nil)
;; (set-face-underline-p 'highlight "#ff0000")

;;;;; hl-numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;;;;; hl-todo
(use-package hl-todo
  :config
  (hl-todo-mode t))

;;;;; beacon
(use-package beacon
  :commands beacon-mode
  :init (beacon-mode t)
  :bind ("C-S-l" . 'beacon-blink)
  :config
  (setq
   beacon-blink-when-window-changes t  ; only flash on window/buffer changes...
   beacon-blink-when-window-scrolls nil
   beacon-blink-when-point-moves nil
   beacon-dont-blink-commands nil
   beacon-blink-when-focused t
   beacon-blink-duration .5
   beacon-blink-delay .5
   beacon-push-mark 1
   beacon-color "#50D050"
   beacon-size 20)
  :delight)

;;________________________________________________________________
;;;    Settings
;;________________________________________________________________
;; By default emacs will not delete selection text when typing on it, let's fix it
(delete-selection-mode t)
;; find-file-at-point, smarter C-x C-f when point on path or URL
(ffap-bindings)
;; Ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
;; show zero-width characters
;; (set-face-background 'glyphless-char "red")
(electric-indent-mode nil)  ; Auto indentation.
(global-subword-mode 1)     ; Iterate through CamelCase words.
(global-auto-revert-mode 1) ; Automatically revert buffer when it changes on disk.
(mouse-avoidance-mode 'exile)
;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
(global-dash-fontify-mode)
(when window-system (global-prettify-symbols-mode t))

;;;; Modeline

;; (setq frame-title-format
;;       '(""
;;         (:eval
;;          (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;;              (replace-regexp-in-string
;;               ".*/[0-9]*-?" "☰ "
;;               (subst-char-in-string ?_ ?  buffer-file-name))
;;            "%b"))
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;;;; General But Better Defaults
(setq-default
 ad-redefinition-action 'accept     ; Silence warnings for redefinition.
 ;; confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
 cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
 speedbar t                         ; Quick file access with bar.
 ;; backup-by-copying t                ; don't clobber symlinks.
 ;; backup-directory-alist `(("."~/.emacs.d/var/backup/per-session))
 default-directory "~/"
 custom-safe-themes t
 load-prefer-newer t ; don't use the compiled code if its the older package.
 make-backup-files nil               ; backup of a file the first time it is saved.
 delete-by-moving-to-trash t       ; move deleted files to trash.
 ;; delete-old-versions t             ; delete excess backup files silently.
 ;; kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2).
 ;; kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2).
 ;; version-control t                 ; version numbers for backup files.
 auto-save-default t               ; auto-save every buffer that visits a file.
 auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
 auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
 compilation-always-kill t         ; kill compilation process before starting another.
 compilation-ask-about-save nil    ; save all buffers on `compile'.
 compilation-scroll-output t
 tab-width 2
 indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
 indent-line-function 'insert-tab
 require-final-newline t
 x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
 save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
 apropos-do-all t                  ; Shows all options when running apropos.
 mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click.
 message-log-max 1000
 fill-column 80
 make-pointer-invisible t          ; hide cursor when writing.
 column-number-mode t              ; show (line,column) in mode-line.
 cua-selection-mode t              ; delete regions.
 enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
 dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
 backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
 )

(setq
 debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
 jka-compr-verbose init-file-debug
 read-process-output-max (* 64 1024); 64kb
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0              ; default is 0.5.
 scroll-step 1                      ; scroll with less jump.
 scroll-preserve-screen-position t
 scroll-margin 3
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
 auto-window-vscroll nil            ; Lighten vertical scroll.
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 hscroll-step 1                     ; Horizontal Scroll.
 hscroll-margin 1
 help-window-select t               ; select help window when opened
 redisplay-skip-fontification-on-input t
 tab-always-indent 'complete        ; smart tab behavior - indent or complete.
 visible-bell t                     ; Flash the screen on error, don't beep.
 view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
 use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
 echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
 delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
 kill-whole-line t 			        ; kills the entire line plus the newline
 save-place-forget-unreadable-files nil
 blink-matching-paren t             ; Blinking parenthesis.
 next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
 require-final-newline t            ; require final new line.
 mouse-sel-retain-highlight t       ; keep mouse high-lighted.
 highlight-nonselected-windows nil
 transient-mark-mode t              ; highlight the stuff you are marking.
 ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
 pgtk-wait-for-event-timeout 0.001
 display-line-numbers-type 'relative
 speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
 frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b") ; name of the file I am editing as the name of the window.
 )
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(ob-sql-mode dirvish bug-hunter focus org-recur org-agenda-property zygospore which-key web-mode volatile-highlights use-package typescript-mode treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons tree-sitter-langs tide theme-changer telega solaire-mode sideline-flycheck saveplace-pdf-view rust-playground rust-mode reverse-im rainbow-delimiters prettier-js pbcopy parrot ox-reveal ox-hugo org-transclusion org-super-agenda org-roam-ui org-roam-timestamps org-roam-bibtex org-ref org-re-reveal org-noter-pdftools org-modern org-download org-cliplink org-caldav org-appear org-alert olivetti ob-typescript ob-rust multi-vterm minions magit-todos lsp-ui ligature kind-icon json-mode js2-mode indent-guide import-js highlight-numbers highlight-indent-guides helm-lsp helm-bibtex gruvbox-theme go-mode git-gutter-fringe general fzf format-all flycheck-rust flycheck-inline fancy-battery evil-collection emojify ement elfeed doom-themes doom-modeline djvu dired-single dired-sidebar dired-rainbow dired-open dashboard dap-mode corfu company-org-block company-box company-bibtex company-auctex citar-org-roam citar-embark cargo blamer beacon apheleia ac-math)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modern-horizontal-rule ((t (:inherit org-hide :strike-through "light sky blue")))))
