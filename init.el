;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
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
;; -AutoGC

;; emacsclient --no-wait--alternate-editor=emacs [FILE]
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Generic packages
(require 'package)
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/"))
      package-quickstart nil)
;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/"))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("melpa-stable" . 3)
        ("org" . 2)
        ("gnu" . 1)))

;; Configure Package Manager
;; (unless (bound-and-true-p package--initialized)
;;   (setq package-enable-at-startup nil) ; To prevent initializing twice
;;   (package-initialize))

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use-package
;; Install use-package if not installed
(eval-and-compile
  (unless (and (fboundp 'package-installed-p)
               (package-installed-p 'use-package))
    (package-refresh-contents) ; update archives
    (package-install 'use-package)) ; grab the newest use-package
  (if init-file-debug
      (setq use-package-compute-statistics t)
    (setq use-package-compute-statistics nil))
  (require 'use-package))

;; Configure use-package
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)  ; :ensure t by default
  (use-package-always-defer t) ; :defer t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

;; ─────────────────── Additional Packages and Configurations ──────────────────
;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

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
;;    Setup config using org-mode
;;________________________________________________________________
;; (org-babel-load-file
;;  (expand-file-name
;;   "README.org"
;;   user-emacs-directory))

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
      idle-update-delay                 1.2    ;; Speed things up by not updating so often ; default is 0.5.
      read-process-output-max           (* 8 1024 1024)
      ediff-split-window-function       'split-window-horizontally
      highlight-nonselected-windows     t
      auto-mode-case-fold               nil
      ;; backup-by-copying                 t
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      ;; fast-but-imprecise-scrolling      t ; nil
      jit-lock-defer-time               0.0
      echo-keystrokes                   0.2
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      use-dialog-box                    nil
      load-prefer-newer                 t
      word-wrap                         nil
      bidi-display-reordering           nil
      large-file-warning-threshold nil      ;; Disable "File is large. Really open?"
      x-stretch-cursor                  t   ;; stretch cursor on tabs
      ;; scroll-margin                     4   ;; scroll N to screen edge
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960  ;; x 10 (960mb), (Emacs uses x100), but this seems too high.

      debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
      jka-compr-verbose init-file-debug
      ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
      ;; scroll-step 3                      ; scroll with less jump.
      ;; scroll-preserve-screen-position t
      ;; scroll-margin 3
      ;; scroll-conservatively 101
      ;; scroll-up-aggressively 0.1
      ;; scroll-down-aggressively 0.1
      ;; lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
      ;; auto-window-vscroll nil            ; Lighten vertical scroll.
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ;; mouse-wheel-progressive-speed nil
      ;; hscroll-step 1                     ; Horizontal Scroll.
      ;; hscroll-margin 1
      help-window-select t               ; select help window when opened
      redisplay-skip-fontification-on-input t
      tab-always-indent 'complete        ; smart tab behavior - indent or complete.
      visible-bell t                     ; Flash the screen on error, don't beep.
      view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
      use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
      delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
      kill-whole-line t 			        ; kills the entire line plus the newline
      save-place-forget-unreadable-files nil
      blink-matching-paren t             ; Blinking parenthesis.
      next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
      require-final-newline t            ; require final new line.
      mouse-sel-retain-highlight t       ; keep mouse high-lighted.
      transient-mark-mode t              ; highlight the stuff you are marking.
      ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
      pgtk-wait-for-event-timeout 0.001
      display-line-numbers-type 'relative
      speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
      frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b") ; name of the file I am editing as the name of the window.

      cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
      default-directory "~/"
      custom-safe-themes t
      load-prefer-newer t ; don't use the compiled code if its the older package.
      make-backup-files t               ; backup of a file the first time it is saved.
      delete-by-moving-to-trash t       ; move deleted files to trash.
      delete-old-versions t             ; delete excess backup files silently.
      kept-new-versions 6               ; newest versions to keep when a new numbered backup is made (default: 2).
      kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2).
      version-control t                 ; version numbers for backup files.
      auto-save-default t               ; auto-save every buffer that visits a file.
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
      compilation-always-kill t         ; kill compilation process before starting another.
      compilation-ask-about-save nil    ; save all buffers on `compile'.
      compilation-scroll-output t
      tab-width 4
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
      backward-delete-char-untabify-method 'hungry) ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Disable backup
(setq backup-inhibited t)
;; Disable auto save
(setq auto-save-default nil)

(setq frame-resize-pixelwise t)
(dotimes (n 3)
  (toggle-frame-maximized))






;;
;;; Reasonable defaults for interactive sessions

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
(add-hook 'doom-first-buffer-hook #'gcmh-mode)


;;; Disable UI elements early
;; PERF,UI: Doom strives to be keyboard-centric, so I consider these UI elements
;;   clutter. Initializing them also costs a morsel of startup time. Whats more,
;;   the menu bar exposes functionality that Doom doesn't endorse. Perhaps one
;;   day Doom will support these, but today is not that day.
;;
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because they do extra work to manipulate frame variables
;;   that isn't necessary this early in the startup process.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)










;; (use-package ox-hugo
;;   ;;Auto-install the package from Melpa
;;   :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
;;   :after ox
;;   :config
;;   (use-package org-re-reveal  )
;;   (use-package ox-reveal  )
;;   (setq org-reveal-root "file:~/Org/Presentations/reveal.js/"))

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cl-lib  )
(setq sql-sqlite-program "/usr/bin/sqlite3")

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

(use-package vterm
  :demand t)

(use-package multi-vterm
  :ensure t
  :after vterm
  :bind
  ("C-x q" . vterm-clear)
  ("C-x w" . multi-vterm))

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

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c e") #'emojify-insert-emoji)) ; override binding in any mode

(use-package pbcopy)

(use-package zygospore
  :bind
  (:map global-map
        ("C-x 1" . zygospore-toggle-delete-other-windows)))

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

(global-set-key (kbd "\C-c f") 'format-all-buffer)

(xterm-mouse-mode t)

;; (add-hook 'prog-mode-hook 'linum-mode)
;; (add-hook 'prog-mode-hook 'visual-line-mode)
;; (add-hook 'prog-mode-hook 'show-paren-mode)
;; (add-hook 'prog-mode-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;      SPELLING       ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;________________________________________________________________
;;;    Flycheck / Flyspell
;;________________________________________________________________

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.5)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 0.5)
	:config
	;; enable typescript-tslint checker
	(flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
;;
;; (use-package ispell
;;   :bind ("<f8>" . ispell-word) ; easy spell check
;;   :custom
;;   (ispell-program-name "hunspell") ; require Hunspell
;;   (ispell-dictionary "en_US,en_GB,ru_RU")
;;   (ispell-personal-dictionary "~/.emacs.d/.hunspell_personal")
;;   :config
;;   ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
;;   ;; dictionary' even though multiple dictionaries will be configured
;;   ;; in next line.
;;   (setenv "LANG" "en_US.UTF-8")
;;   ;; ispell-set-spellchecker-params has to be called
;;   ;; before ispell-hunspell-add-multi-dic will work
;;   (ispell-set-spellchecker-params)
;;   (ispell-hunspell-add-multi-dic ispell-dictionary)
;;   (unless (file-exists-p ispell-personal-dictionary)
;;     (write-region "" nil ispell-personal-dictionary nil 0)))
;;
;; (use-package flyspell
;;   :bind (:map flyspell-mode-map
;;               ("C-;"        . nil)
;;               ("C-,"        . nil)
;;               ("C-."        . nil)
;;               ("M-TAB"      . nil)
;;               ("C-x M-$"    . flyspell-buffer)
;;               ("C-<f7>"     . flyspell-auto-correct-word)
;;               ("C-<f12>"    . flyspell-auto-correct-previous-word))
;;   :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
;;                  (add-hook hook 'turn-on-flyspell)))
;;                ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;   :delight " ⓢ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;      SOCIALS       ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;    Matrix
;;________________________________________________________________
(use-package ement)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;     NAVIGATION     ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
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

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

(use-package lsp-treemacs
  :after treemacs
  :demand t
  :config
  (lsp-treemacs-sync-mode 1))

;;________________________________________________________________
;;    Projectile
;;________________________________________________________________

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-globally-ignored-files "node_modules"))

(defun my/highlight-todo-like-words ()
  (font-lock-add-keywords
   nil `(("\\<\\(FIXME\\|TODO\\)"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'my/highlight-todo-like-words)

;;________________________________________________________________
;;    Helm
;;________________________________________________________________

(use-package helm
  :init
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  :demand t
  :config
  (setq
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
(helm-mode 1)

;;________________________________________________________________
;;    Dashboard
;;________________________________________________________________
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
  (setq dashboard-items '((recents  . 8)
                          (agenda   . 5)
                          (projects . 6)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;     PDF READER     ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;________________________________________________________________
;;;;    Fancy pkg
;;________________________________________________________________
;; (use-package fancy-battery
;;   :config
;;   (setq fancy-battery-show-percentage t)
;;   (setq battery-update-interval 15)
;;   (if window-system
;;       (fancy-battery-mode)
;;     (display-battery-mode)))

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

;;;; Load custom-files
(defun load-directory (dir)
  "Load all *.el files in a directory."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory "~/.emacs.d/config") ; load my configuration of packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("/home/chopin/Org/agenda/PlanAhead.org" "/home/chopin/Org/agenda/PlannedDay.org"))
 '(package-selected-packages
   '(org-journal zygospore which-key web-mode volatile-highlights use-package typescript-mode treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons tree-sitter-langs timu-rouge-theme tide theme-changer telega solaire-mode smooth-scrolling sideline-flycheck saveplace-pdf-view rust-playground rust-mode reverse-im rainbow-delimiters quelpa prettier-js pbcopy org-wild-notifier org-super-agenda org-roam-ui org-roam-timestamps org-recur org-rainbow-tags org-noter-pdftools org-modern org-fancy-priorities org-download org-cliplink org-books org-appear org-alert olivetti ob-typescript ob-sql-mode ob-rust multi-vterm modus-themes minions magit-todos lsp-ui ligature kind-icon json-mode indent-guide import-js highlight-numbers highlight-indent-guides helm-lsp gruvbox-theme go-mode git-gutter-fringe general format-all focus flycheck-rust flycheck-inline fancy-battery evil-collection emojify ement eglot doom-themes doom-modeline dirvish diredfl dired-single dired-sidebar dired-rainbow dired-open dashboard dap-mode corfu company-org-block company-box company-auctex cargo bug-hunter blamer beacon apheleia ac-math)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
