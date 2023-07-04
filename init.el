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
;;    Install use-package if not installed
;;________________________________________________________________
(eval-and-compile
  (unless (and (fboundp 'package-installed-p)
               (package-installed-p 'use-package))
    (package-refresh-contents) ; update archives
    (package-install 'use-package)) ; grab the newest use-package
  (if init-file-debug
      (setq use-package-compute-statistics t)
    (setq use-package-compute-statistics nil))
  (require 'use-package))

;;________________________________________________________________
;;    Configure use-package
;;________________________________________________________________
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)  ; :ensure t by default
  (use-package-always-defer nil) ; :defer t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

;;________________________________________________________________
;;    Install straight.el
;;________________________________________________________________
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)

;;________________________________________________________________
;;    Install quelpa
;;________________________________________________________________
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; (org-babel-load-file
;;  (expand-file-name
;;   "README.org"
;;   user-emacs-directory))

;; (setq default-frame-alist '((undecorated . nil)))

(add-to-list 'load-path "~/.emacs.d/local-packages/hyp-to-org")

;;________________________________________________________________
;;    Transparent Emacs
;;________________________________________________________________
;; (set-frame-parameter (selected-frame) 'alpha-background '(80 . 80))
;; (add-to-list 'default-frame-alist '(alpha-background . (80 . 80)))

(set-frame-parameter nil 'alpha-background 80)
;; (add-to-list 'default-frame-alist '(alpha-background . 80))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)

;; Use the following snippet after you’ve set the alpha as above to assign a toggle to “C-c t b”:
(defun toggle-transparency ()
  "Crave for transparency!"
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql (cond ((numberp alpha-background) alpha-background)
                    ((numberp (cdr alpha-background)) (cdr alpha-background))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha-background)) (cadr alpha-background)))
              100)
         '(80 . 100) '(100 . 100)
         ))))
(global-set-key (kbd "C-c t b") 'toggle-transparency)

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		  ; Show battery.
(display-time-mode t)			  ; Show time.
(set-fringe-mode 1)               ; Give us some space.
(delete-selection-mode nil)		  ; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)     ; Set yes or no to y/n
(global-font-lock-mode 1)         ; always highlight code
(global-auto-revert-mode 1)       ; refresh a buffer if changed on disk
(global-hl-line-mode 1)           ; Highlight current line
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
(setq-default shell-file-name "/usr/bin/fish")

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

(use-package go-mode :ensure t)

;; (load "~/.emacs.d/local-packages/epubmode")
;; (require 'epubmode)

;; (load "~/.emacs.d/local-packages/company-go")
;; (require 'company-go)

;; (load "~/.emacs.d/local-packages/chep-video")
;; (require 'chep-video)

;; (load "~/.emacs.d/local-packages/dired+")
;; (require 'dired+)

;; (load "~/.emacs.d/local-packages/nov")
;; (require 'nov)

;; (use-package nov :ensure t)

;;(add-to-list 'load-path "~/.emacs.d/local-themes/catppucin-macchiato-theme")

(use-package pbcopy
  :ensure t)

;; Disable backup
(setq backup-inhibited t)
;; Disable auto save
(setq auto-save-default nil)

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))

(setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

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
  ;;:straight nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t))

(set-face-attribute 'default t
                    :font "Iosevka" ;; Terminess Nerd Font Propo, Input, Terminess Nerd Font Propo
                    :height 100
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 100
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 100
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
(add-to-list 'default-frame-alist '(font . "Iosevka 11"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka 11"))

(add-to-list 'default-frame-alist '(font . "Iosevka 11"))
;; Changes certain keywords to symbols, such as lamda
(setq global-prettify-symbols-mode t)

(use-package theme-changer
  :ensure t
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88)

  (use-package gruvbox-theme
    :ensure t)

  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "all-the-icons") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  ;; (change-theme 'doom-one-light 'gruvbox-dark-medium)
  ;; (change-theme 'doom-ayu-light 'doom-ayu-dark)
  ;; (load-theme 'gruvbox-dark-medium t)
  ;; (load-theme 'doom-one t)
  (change-theme 'doom-one-light 'doom-one))

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
     org-insert-heading-respect-content t
     ;; Appearance
     org-hide-leading-stars t
     org-startup-indented nil
     org-modern-radio-target    '("❰" t "❱")
     org-modern-internal-target '("↪ " t "")
     org-modern-todo t
     org-modern-tag t
     org-modern-timestamp t
     org-modern-statistics t
     ;; org-modern-table nil
     org-modern-progress t
     org-modern-priority t
     org-modern-horizontal-rule "──────────"
     org-modern-hide-stars "·"
     ;; org-modern-star ["⁖"]
     ;; org-modern-list '((43 . "•")
     ;;                   (45 . "–")
     ;;                   (42 . "∘"))))
     org-modern-keyword "‣"))

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

  (use-package org-padding
    :quelpa (org-padding :repo "TonCherAmi/org-padding" :fetcher github)
    :config
    (setq org-padding-block-begin-line-padding '(2.0 . nil))
    (setq org-padding-block-end-line-padding '(nil . 1.0))
    (setq org-padding-heading-padding-alist
          '((4.0 . 1.5) (3.0 . 0.5) (3.0 . 0.5) (3.0 . 0.5) (2.5 . 0.5) (2.0 . 0.5) (1.5 . 0.5) (0.5 . 0.5))))

  ;; (use-package svg-tag-mode
  ;;   :config
  ;;   (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  ;;   (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  ;;   (defconst day-re "[A-Za-z]\\{3\\}")
  ;;   (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  ;;   (defun svg-progress-percent (value)
  ;;     (svg-image (svg-lib-concat
  ;;                 (svg-lib-progress-bar
  ;;                  (/ (string-to-number value) 100.0) nil
  ;;                  :height 0.8 :foreground (doom-color 'fg) :background (doom-color 'bg)
  ;;                  :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
  ;;                 (svg-lib-tag (concat value "%") nil
  ;;                              :height 0.8 :foreground (doom-color 'fg) :background (doom-color 'bg)
  ;;                              :stroke 0 :margin 0)) :ascent 'center))

  ;;   (defun svg-progress-count (value)
  ;;     (let* ((seq (mapcar #'string-to-number (split-string value "/")))
  ;;            (count (float (car seq)))
  ;;            (total (float (cadr seq))))
  ;;       (svg-image (svg-lib-concat
  ;;                   (svg-lib-progress-bar (/ count total) nil
  ;;                                         :foreground (doom-color 'fg)
  ;;                                         :background (doom-color 'bg) :height 0.8
  ;;                                         :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
  ;;                   (svg-lib-tag value nil
  ;;                                :foreground (doom-color 'fg)
  ;;                                :background (doom-color 'bg)
  ;;                                :stroke 0 :margin 0 :height 0.8)) :ascent 'center)))

  ;;   (set-face-attribute 'svg-tag-default-face nil :family "Alegreya Sans")
  ;;   (setq svg-tag-tags
  ;;         `(;; Progress e.g. [63%] or [10/15]
  ;;           ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
  ;;                                               (svg-progress-percent (substring tag 1 -2)))))
  ;;           ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
  ;;                                             (svg-progress-count (substring tag 1 -1)))))
  ;;           ;; Task priority e.g. [#A], [#B], or [#C]
  ;;           ("\\[#A\\]" . ((lambda (tag) (svg-tag-make tag :face 'error :inverse-video t :height .85
  ;;                                                      :beg 2 :end -1 :margin 0 :radius 10))))
  ;;           ("\\[#B\\]" . ((lambda (tag) (svg-tag-make tag :face 'warning :inverse-video t :height .85
  ;;                                                      :beg 2 :end -1 :margin 0  :radius 10))))
  ;;           ;; Keywords
  ;;           ("TODO" . ((lambda (tag) (svg-tag-make tag :inverse-video t :height .85 :face 'org-todo))))
  ;;           ("HOLD" . ((lambda (tag) (svg-tag-make tag :height .85 :face 'org-todo))))
  ;;           ("DONE\|STOP" . ((lambda (tag) (svg-tag-make tag :inverse-video t :height .85 :face 'org-done))))
  ;;           ("NEXT\|WAIT" . ((lambda (tag) (svg-tag-make tag :inverse-video t :height .85 :face '+org-todo-active))))
  ;;           ("REPEAT\|EVENT\|PROJ\|IDEA" .
  ;;            ((lambda (tag) (svg-tag-make tag :inverse-video t :height .85 :face '+org-todo-project))))
  ;;           ("REVIEW" . ((lambda (tag) (svg-tag-make tag :inverse-video t :height .85 :face '+org-todo-onhold))))))

  ;;   (add-hook 'org-mode-hook 'svg-tag-mode))

  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t
          org-appear-autolinks 'just-brackets))

  (with-eval-after-load 'org
    (setq org-log-done 'time))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ;What needs to be done
           "NEXT(n)"                    ;A project without NEXTs is stuck
           "|"
           "DONE(d)")
          (sequence
           "REPEAT(e)"                    ;Repeating tasks
           "|"
           "DONE")
          (sequence
           "HOLD(h)"                    ;Task is on hold because of me
           "PROJ(p)"                    ;Contains sub-tasks
           "WAIT(w)"                    ;Tasks delegated to others
           "REVIEW(r)"                  ;Daily notes that need reviews
           "IDEA(i)"                    ;Daily notes that need reviews
           "|"
           "STOP(c)"                    ;Stopped/cancelled
           "EVENT(m)"                   ;Meetings
           ))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("REVIEW" . +org-todo-onhold)
          ("HOLD" . +org-todo-cancel)
          ("PROJ" . +org-todo-project)
          ("DONE"   . +org-todo-cancel)
          ("STOP" . +org-todo-cancel)))

  (setq org-clock-sound "~/.emacs.d/sounds/sound.wav")

  (use-package org-alert
    :ensure t)

  (use-package ob-typescript
    :ensure t)

  (use-package ob-rust
    :ensure t)

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
     (latex . t)))

  ;; (add-hook 'org-mode-hook (defun my/pretty-symbols ()
  ;;                            (setq prettify-symbols-alist
  ;;                                  '(("#+begin_src rust" . "🦀")
  ;;                                    ("#+begin_src emacs-lisp" . "λ")
  ;;                                    ("#+begin_src typescript" . " 🔨")
  ;;                                    ("#+begin_src js" . " 🔨")
  ;;                                    ("#+begin_src С" . "🔨")
  ;;                                    ("#+begin_src python" . "🐍")
  ;;                                    ("#+begin_quote" . "❝")
  ;;                                    ("#+end_quote" . "❞")
  ;;                                    ("#+end_src" . "―")
  ;;                                    ("#+results:" . "🔨")
  ;;                                    ("#+RESULTS:" . "🔨")))))

  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            "Beautify Org Checkbox Symbol"
  ;;                            (push '("[ ]" .  "☐") prettify-symbols-alist)
  ;;                            (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;;                            (push '("[-]" . "❍" ) prettify-symbols-alist)
  ;;                            (push '("#+begin_src rust" . "🦀" ) prettify-symbols-alist)
  ;;                            (push '("#+begin_quote" . "❝" ) prettify-symbols-alist)
  ;;                            (push '("#+end_quote" . "❞" ) prettify-symbols-alist)
  ;;                            (prettify-symbols-mode)))

  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;; ;; ORG ROAM SETTING ;; ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("d" "Default abstract" plain "%?"
      :if-new (file+head "%<%Y-%m-%d-%H:%M:%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("s" "Abstract with source" plain "\n\nSource: %^{Source}\n\nTitle: ${title}\n\n"
      :if-new (file+head "%<%Y-%m-%d-%H:%M:%S>-${slug}.org" "#+title: ${title}\n#+date: %U")
      :unnarrowed t)
     ("b" "Books" plain "\n* Source\n\nAuthor: %^{Author}\n\nTitle: ${title}\n\nYear: %^{Year}\n\n"
      :if-new (file+head "%<%Y-%m-%d-%H:%M:%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :Books: :%^{Book type}:\n")
      :unnarrowed t)
     ("e" "Encrypt note" plain "%?"
      :target (file+head "${name-of-file}.org.gpg"
                         "#+title: ${title}\n#+date: %U")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ;; ("C-c n g" . org-roam-graph) ;; Require graphviz package
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c g" . org-id-get-create)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  ;; Org-roam interface
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the node's TITLE, as well as it's HIERACHY."
    (let* ((title (org-roam-node-title node))
           (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
           (level (org-roam-node-level node))
           (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
           (filetitle-or-name (if filetitle filetitle (file-name-nondirectory (org-roam-node-file node))))
           (shortentitle (if (> (length filetitle-or-name) 20) (concat (substring filetitle-or-name 0 20)  "...") filetitle-or-name))
           (separator (concat " " (all-the-icons-material "chevron_right") " ")))
      (cond
       ((= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dyellow))
                            (propertize shortentitle 'face 'org-roam-olp) separator title))
       ((= level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dsilver))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       ((> level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'org-roam-olp))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                  (if filetitle title (propertize filetitle-or-name 'face 'all-the-icons-dyellow)))))))
  )

(use-package org-roam-ui
  :ensure t
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq orui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil
        ))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :config
  (push 'company-org-roam company-backends))

(use-package org-download
  :ensure t)

(setq-default org-download-image-dir "./assets-org/")

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package org-noter
  :ensure t)

;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;; ;;
;; ;; CITES ;; ;;
;; ;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

  - tags ::
  - keywords :: ${keywords}

  \n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t)))
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

;; (use-package org-ref :ensure t
;;   :config
;;   (setq reftex-default-bibliography '("~/Org/2Brain/bibtex/ref.bib"))

;;   (setq org-ref-bibliography-notes "~/Org/2Brain/bibtex/ref_notes.org"
;;         org-ref-default-bibliography '("~/Org/2Brain/ref.bib")
;;         org-ref-pdf-directory "~/Org/2Brain/bibtex/bibtex-pdfs/")

;;   (setq bibtex-completion-bibliography "~/Org/2Brain/bibtex/ref.bib"
;;         bibtex-completion-library-path "~/Org/2Brain/bibtex/bibtex-pdfs/"
;;         bibtex-completion-notes-path "~/Org/2Brain/bibtex/bibtex-notes")

;;   ;; Optional. Open pdf in external viewer.
;;   ;; (setq bibtex-completion-pdf-open-function
;;   ;;       (lambda (fpath)
;;   ;;         (start-process "open" "*open*" "open" fpath)))
;;   ;; )

;; (use-package citar
;;   :config
;;   (setq
;;    citar-bibliography (list (concat org-directory "~/Org/References/zotero.bib"))
;;    citar-notes-paths (list(concat org-directory "~/Org/Org-roam/literature/"))
;;    citar-library-paths (list (concat org-directory "~/Org/Org-roam/"))
;;    citar-file-variable "file"
;;    ;; citar-symbols
;;    `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red :v-adjust -0.1) . " ")
;;      (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
;;      (link ,(all-the-icons-material "link" :face 'all-the-icons-blue) . " "))
;;    citar-symbol-separator "  "
;;    org-cite-global-bibliography citar-bibliography))

;; Search contents of PDFs
;; (after! (embark pdf-occur)
;;         (defun citar/search-pdf-contents (keys-entries &optional str)
;;           "Search pdfs."
;;           (interactive (list (citar-select-refs)))
;;           (let ((files (citar-file--files-for-multiple-entries
;;                         (citar--ensure-entries keys-entries)
;;                         citar-library-paths
;;                         '("pdf")))
;;                 (search-str (or str (read-string "Search string: "))))
;;             (pdf-occur-search files search-str t)))
;;         ;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
;;         (add-to-list 'embark-multitarget-actions #'citar/search-pdf-contents))

;; (use-package citar-embark
;;   :ensure t
;;   :after citar embark
;;   :no-require
;;   :config
;;   (org-cite-global-bibliography
;;    '("~/Org/Org-roam/References/zotero.bib"))
;;   (citar-embark-mode))

;; Use `citar' with `org-cite'
(use-package citar-org-roam
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (setq citar-org-roam-subdir "literature"
        citar-org-roam-note-title-template
        (string-join
         '("${author editor} (${year issued date}) ${title}"
           "#+filetags: literature"
           "#+startup: overview"
           "#+startup: hideblocks"
           "#+options: toc:2 num:t"
           ""
           "* What?"
           "* Why?"
           "* How?"
           "* And?"
           ) "\n"))
  (citar-org-roam-mode)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;; ORG PDF, ORG NOTER ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link)
  )
(use-package org-noter-pdftools
  :ensure t
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

(setq org-agenda-files   (list "~/Org")
      org-log-done 'time)

(setq who/org-agenda-directory "~/Org/agenda")

(require 'find-lisp)
(defun who/find-org-files (directory)
  (find-lisp-find-files directory "\.org$"))

(defun who-org/agenda-files-update (&rest _)
  (let ((todo-zettels (->> "rg --files-with-matches '(TODO)|(NEXT)|(DONE)|(REPEAT)|(DONE)|(HOLD)|(PROJ)|(WAIT)|(REVIEW)|(IDEA)|(STOP)|(EVENT)' ~/Org/Org-roam/"
                           (shell-command-to-string)
                           (s-lines)
                           (-filter (lambda (line) (not (s-blank? line)))))))
    (setq org-agenda-files (append (who/find-org-files who/org-agenda-directory)
                                   todo-zettels))))

(advice-add 'org-agenda :before #'who-org/agenda-files-update)

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator #x2501
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)
(with-eval-after-load 'org-journal
  (define-key org-journal-mode-map (kbd "<C-tab>") 'yas-expand))
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
(setq org-agenda-deadline-faces
      '((1.0001 . org-warning)              ; due yesterday or before
        (0.0    . org-upcoming-deadline)))  ; due today or later(setq-default org-icalendar-include-todo t)
(setq org-combined-agenda-icalendar-file "~/Org/calendar.ics")
;; (icalendar-import-file "~/Org/calendar.ics" "diary-google")
(setq org-icalendar-combined-name "Hugo Org")
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "Asia/Vladivostok")
(setq org-icalendar-store-UID t)
(setq org-icalendar-alarm-time 30)
(setq calendar-date-style 'european
      calendar-mark-holidays-flag t
      calendar-week-start-day 1
      calendar-mark-diary-entries-flag nil)

(alert-define-style 'who/alert-style-reminder
                    :title "Agenda reminders"
                    :notifier (lambda (info)
                                (alert-libnotify-notify (plist-put info :persistent t))))

(add-to-list 'alert-user-configuration
             '(((:title . "Agenda"))
               who/alert-style-reminder))

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
(setq org-cycle-separator-lines 2)
;; (setq org-agenda-category-icon-alist
;;       `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
;;         ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
;;         ("Calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
;;         ("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))

(use-package org-super-agenda :ensure t)

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
                                 :and (:priority>= "B" :category "Work" :todo ("TODO" "NEXT")))
                          (:name "Work other"
                                 :and (:category "Work" :todo ("TODO" "NEXT")))
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
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

(setq org-directory "~/Org")
(setq org-default-notes-file "~/Org/agenda/notes.org")

(setq org-capture-templates
      '(
        ("t" "TODO" entry
         (file "~/Org/agenda/inbox.org") "* TODO %^{Title}")
        ("m" "Meeting notes" entry
         (file "~/Org/agenda/appointments.org") "* TODO %^{Title} %t\n- %?")
        ("w" "Work TODO" entry
         (file "~/Org/agenda/work.org") "* TODO %^{Title}")
        ("d" "Diary" entry (file "~/Org/diary.org.gpg")
         "* %U\n" :clock-in t :clock-resume t) ;; "*** %?\n%U\n" :clock-in t :clock-resume t)
        ("n" "Notes" entry
         (file "~/Org/agenda/inbox.org") "* %^{Description} %^g\n Added: %U\n%?")
        ))

(use-package org-caldav
  :ensure t
  :custom
  (org-caldav-url "https://lunarcloud.ddns.net/remote.php/dav/calendars/ncp")
  (org-caldav-calendar-id "personal")
  (org-caldav-inbox "~/Org/agenda/cal_inbox.org")
  (org-caldav-files '("~/Org/agenda/calendar.org"))
  (org-icalendar-timezone "Asia/Vladivostok")
  (org-caldav-delete-org-entries 'never))
;; (org-caldav-sync)

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(use-package org-re-reveal :ensure t)
(use-package ox-reveal :ensure t)

(setq org-reveal-root "file:~/Org/Presentations/reveal.js/")

(use-package ement
  :ensure t)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "ru_RU")
(setq ispell-local-dictionary-alist
      '(("ru_RU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

;; (require 'ox-latex)
;; (setq org-latex-create-formula-image-program 'dvipng)
;; (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; Set up default LaTeX preview configuration
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
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil) ; adjust to your liking

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package pdf-tools
  :ensure t
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  ;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
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

(use-package saveplace-pdf-view :ensure t)
(save-place-mode 1)

;; (load "~/.emacs.d/local-packages/nov-xwidget")
;; (require 'nov-xwidget)

(use-package cl-lib :ensure t)

;; Best .epub reader
;; (use-package nov-xwidget
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

(setq sql-sqlite-program "/usr/bin/sqlite3")
;; (setq calibredb-program "/Applications/calibre.app/Contents/MacOS/calibredb")

(use-package calibredb
  :ensure t
  :defer t
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Books")))
  (setq calibredb-virtual-library-alist '(("1. Development - work" . "work \\(pdf\\|epub\\)")
                                          ("2. Read it later" . "Readit epub")
                                          ("3. Development - rust" . "rust")))
  (setq calibredb-format-all-the-icons t)
  (setq calibredb-format-icons-in-terminal t))

;; Keybindings

(defvar calibredb-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'calibredb-entry-dispatch)
    (define-key map "o" #'calibredb-find-file)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "V" #'calibredb-open-file-with-default-tool)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "q" #'calibredb-entry-quit)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-open-dired)
    (define-key map "\M-/" #'calibredb-rga)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-show-mode'.")

(defvar calibredb-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'calibredb-search-mouse)
    (define-key map (kbd "<RET>") #'calibredb-find-file)
    (define-key map "?" #'calibredb-dispatch)
    (define-key map "a" #'calibredb-add)
    (define-key map "A" #'calibredb-add-dir)
    (define-key map "c" #'calibredb-clone)
    (define-key map "d" #'calibredb-remove)
    (define-key map "D" #'calibredb-remove-marked-items)
    (define-key map "j" #'calibredb-next-entry)
    (define-key map "k" #'calibredb-previous-entry)
    (define-key map "l" #'calibredb-virtual-library-list)
    (define-key map "L" #'calibredb-library-list)
    (define-key map "n" #'calibredb-virtual-library-next)
    (define-key map "N" #'calibredb-library-next)
    (define-key map "p" #'calibredb-virtual-library-previous)
    (define-key map "P" #'calibredb-library-previous)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "S" #'calibredb-switch-library)
    (define-key map "o" #'calibredb-find-file)
    (define-key map "O" #'calibredb-find-file-other-frame)
    (define-key map "v" #'calibredb-view)
    (define-key map "V" #'calibredb-open-file-with-default-tool)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-open-dired)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "b" #'calibredb-catalog-bib-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "r" #'calibredb-search-refresh-and-clear-filter)
    (define-key map "R" #'calibredb-search-clear-filter)
    (define-key map "q" #'calibredb-search-quit)
    (define-key map "m" #'calibredb-mark-and-forward)
    (define-key map "f" #'calibredb-toggle-favorite-at-point)
    (define-key map "x" #'calibredb-toggle-archive-at-point)
    (define-key map "h" #'calibredb-toggle-highlight-at-point)
    (define-key map "u" #'calibredb-unmark-and-forward)
    (define-key map "i" #'calibredb-edit-annotation)
    (define-key map (kbd "<DEL>") #'calibredb-unmark-and-backward)
    (define-key map (kbd "<backtab>") #'calibredb-toggle-view)
    (define-key map (kbd "TAB") #'calibredb-toggle-view-at-point)
    (define-key map "\M-n" #'calibredb-show-next-entry)
    (define-key map "\M-p" #'calibredb-show-previous-entry)
    (define-key map "/" #'calibredb-search-live-filter)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-search-mode'.")

;; Setting dashboard
(use-package dashboard
  :ensure t
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-banner-logo-title "Welcome back, Darling!"
        dashboard-startup-banner "~/.emacs.d/images/emacs-e-small.png"
        ;; dashboard-page-separator ""
        dashboard-set-navigator t
        dashboard-items '(
                          (recents . 6)
                          ;; (agenda . 4 )
                          ;;(registers . 3)
                          (bookmarks . 4)
                          (projects . 4))) ;; use standard emacs logo as banner

  ;; Format: "(icon title help action face prefix suffix)"
  ;; (setq dashboard-navigator-buttons
  ;; 			`(;; line1
  ;; 				((,(all-the-icons-wicon "tornado" :height 1.1 :v-adjust 0.0)
  ;; 					"Main site"
  ;; 					"Browse homepage"
  ;; 					(lambda (&rest _) (browse-url "homepage")))
  ;; 				 ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
  ;; 				 ("?" "" "?/h" #'show-help nil "<" ">"))
  ;; 				;; line 2
  ;; 				((,(all-the-icons-faicon "github" :height 1.1 :v-adjust 0.0)
  ;; 					"Github"
  ;; 					""
  ;; 					(lambda (&rest _) (browse-url "homepage")))
  ;; 				 ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
  (setq dashboard-footer-messages '("Richard Stallman is proud of you."))
  ;; (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
  ;; 																									 :height 1.1
  ;; 																									 :v-adjust -0.05
  ;; 																									 :face 'font-lock-keyword-face))
  :config
  ;; (dashboard-modify-heading-icons '((recents . "file-text")
  ;;                                   (bookmarks . "book")))
  (dashboard-setup-startup-hook)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(defun dashboard-refresh-buffer ()
  (interactive)
  (when (get-buffer dashboard-buffer-name)
    (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))

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

(use-package dired
  :ensure nil
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
  (use-package all-the-icons-dired
	  :ensure t
	  :hook (dired-mode . all-the-icons-dired-mode) ; Display icons in Dired mode
	  :init
	  (setq all-the-icons-dired-mode-inline-electric-icons t))) ; Show electric icons for Dired mode

(use-package dired-rainbow
  :ensure t
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
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-checker-simple-format t)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-major-mode-icon nil)
  (doom-line-numbers-style 'relative)
  (doom-modeline-enable-word-count t)
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-vcs-max-length 50)
  (doom-modeline-workspace-name t)
  (doom-modeline-flycheck-icon t)
  (doom-modeline-time-icon nil)
  (doom-modeline-modal-icon t)
  (doom-modeline-height 35)
  (doom-modeline-battery t)
  (doom-modeline-lsp nil)
  (doom-modeline-lsp t))

(use-package minions
  :ensure t
  :delight " 𝛁"
  :hook (doom-modeline-mode . minions-mode)
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]"))

(use-package elfeed
  :ensure t
  :config
  ;; data is stored in ~/.elfeed
  (setq elfeed-feeds
        '(
          ;; freelance
          ("https://freelance.habr.com/user_rss_tasks/vsE2OtRKoyNeUnK7RGd+0w==" freelance)

          ;;
          ("https://habr.com/ru/rss/feed/posts/all/bd769e8234cb6e6444ae3197fd0c0d9b/?fl=ru" habr-my-topics)

          ;; programming
          ;;("https://news.ycombinator.com/rss" hacker)
          ;;("https://www.reddit.com/r/programming.rss" programming)
          ("https://www.reddit.com/r/emacs.rss" emacs)
          ("https://www.opennet.ru/opennews/opennews_all_utf.rss" opennet-news)
          ;; ("https://habr.com/ru/rss/all/all/?fl=ru" habr-all)
          ("https://habr.com/ru/rss/news/?fl=ru" habr-news)
          ("https://nuancesprog.ru/feed" nop)
          ("https://dev.to/feed" dev-to)

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

(use-package elfeed-dashboard
  :ensure t
  :config
  (setq elfeed-dashboard-file "~/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package evil
  :ensure t
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t))

(use-package general
  :ensure t)
(general-evil-setup t)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-emacs-state-cursor '("#FF5D62" box))
  (setq evil-normal-state-cursor '("#FF5D62" box))
  (setq evil-visual-state-cursor '("#98BB6C" box))
  (setq evil-insert-state-cursor '("#E82424" bar))
  (setq evil-replace-state-cursor '("#FF9E3B" hbar))
  (setq evil-operator-state-cursor '("#7E9CD8" hollow))
  (evil-collection-init)
  (evil-mode 1))

(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-set-initial-state 'bookmark-bmenu-mode 'normal)
(evil-set-initial-state 'vterm-mode 'normal)
(evil-set-initial-state 'calibredb-mode 'normal)
;; (evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'sunrise-mode 'emacs)

(use-package fzf
  :ensure t
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
  :ensure t
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
		    magit-todos-depth 4
		    magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*" "node_modules/" "target/"))
  (custom-set-variable
   '(magit-todos-keywords (list "TODO" "FIXME" "BUGFIX" "HACK"))))

(use-package blamer
  :ensure t
  ;; :bind (("s-i" . blamer-show-commit-info)
  ;;        ("C-c i" . ("s-i" . blamer-show-posframe-commit-info)))
  :defer 20
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

;; (use-package blamer
;;   :ensure t
;;   :commands (blamer-mode)
;;   :config
;;   (setq blamer-view 'overlay
;;         blamer-type 'posframe-popup
;;         blamer-max-commit-message-length 70
;;         blamer-force-truncate-long-line nil
;;         blamer-author-formatter " ✎ [%s] - "
;;         blamer-commit-formatter "● %s ● ")
;;   :custom
;;   (blamer-idle-time 1.0)
;;   :custom-face
;;   (blamer-face ((t :foreground "#E46876"
;;                    :height 140
;;                    :italic t))))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-interval 0.5))

(use-package git-gutter-fringe
  :ensure t
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(global-git-gutter-mode +1)

(use-package projectile
  :ensure t
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

(use-package telega
  :ensure t
  :config
  (setq telega-use-docker t)
  (add-hook 'telega-load-hook 'telega-notifications-mode)
  (add-hook 'telega-load-hook 'telega-appindicator-mode)
  (add-hook 'telega-load-hook 'global-telega-url-shorten-mode))

(use-package lsp-treemacs)

(use-package treemacs
  :after lsp-treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") 'treemacs-select-window))
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
    ;; (treemacs-resize-icons 48)

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

(use-package treemacs-all-the-icons
  :ensure t)
(treemacs-load-theme "all-the-icons")

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :bind
  ("C-x q" . vterm-clear)
  ("C-x w" . multi-vterm))

(use-package helm
  :ensure t
  :defer t
  :custom
  (helm-M-x-use-completion-styles nil)
  (helm-split-window-inside-p t)
  (helm-follow-mode-persistent t)
  (helm-buffers-show-icons t)
  :bind (:map helm-map
              ("<tab>" . 'helm-execute-persistent-action))
  :config
  (helm-mode 1))

(with-eval-after-load 'helm
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))

;; Needed for `:after char-fold' to work
(use-package char-fold
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
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

(use-package emojify
  :ensure t
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c e") #'emojify-insert-emoji)) ; override binding in any mode

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package zygospore :ensure t)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(use-package all-the-icons
  :ensure t)

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode))

(defun my/parrot-animate-when-compile-success (buffer result)
  (if (string-match "^finished" result)
	    (parrot-start-animation)))

(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  (parrot-set-parrot-type 'thumbsup)
  (add-hook 'before-save-hook 'parrot-start-animation)
  (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config (solaire-global-mode +1)
  :delight)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
                                        ; Mak;; ESC quit prompts
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
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(lsp-treemacs-sync-mode 1)
(helm-mode 1)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-show-numbers t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-require-match nil
        company-global-modes '(not erc-mode message-mode help-mode)
        company-transformers '(company-sort-by-occurrence))

  ;; Enable company mode in specific modes
  (add-hook 'rust-mode-hook #'company-mode)
  (add-hook 'go-mode-hook #'company-mode)
  (add-hook 'typescript-mode-hook #'company-mode)
  (add-hook 'js-mode-hook #'company-mode)
  (add-hook 'rjsx-mode-hook #'company-mode)

  ;; Set up company backends for each major mode
  (setq company-backends-rust '((company-capf company-files)))
  (setq company-backends-go '((company-capf company-files)))
  (setq company-backends-typescript '((company-tide company-files)))
  (setq company-backends-js '((company-tide company-files)))
  (setq company-backends-react '((company-tide company-files)))

  ;; Set keybindings for company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "RET") 'company-complete-selection))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-bibtex
  :ensure t)

(use-package ac-math
  :ensure t)

(use-package company-auctex
  :ensure t)

(company-auctex-init)

(use-package company-org-block
  :ensure t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package yasnippet :ensure t)

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
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-doc-delay 2
  ;;       lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
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

(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-ui :ensure t)

(use-package web-mode :ensure t
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

(use-package import-js :ensure t)

;; JSX syntax highlighting
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(use-package js2-mode :ensure t :defer 20
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
  :ensure t
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
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(use-package json-mode :ensure t :defer 20
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

(use-package rust-playground :ensure t)

(use-package rust-mode
  :ensure t
  :if (executable-find "rustc"))

(use-package cargo
  :ensure t
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
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;;________________________________________________________________
;;;    Flycheck
;;________________________________________________________________

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.2)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 2))

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
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
  (olivetti-body-width 80)
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
  :ensure t
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
(size-indication-mode)
(setq display-time-24hr-format t
      ;; display-time-format "%l:%M%p" ;  %b %y"
      display-time-default-load-average nil)
(display-time-mode)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

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
