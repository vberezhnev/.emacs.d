;;________________________________________________________________
;;    Base settings of Emacs
;;________________________________________________________________
;; (require 'battery)

;; (funcall battery-status-function)
;; (funcall 'battery-linux-sysfs)

;; (setq display-time-mode t)
;; (setq display-battery-mode t)
(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

;; (set-fringe-mode 1)               ; Give us some space.
(delete-selection-mode nil)		  ; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)     ; Set yes or no to y/n
(global-font-lock-mode 1)         ; always highlight code
(global-auto-revert-mode 1)       ; refresh a buffer if changed on disk
;; (global-hl-line-mode 1)           ; Highlight current line
(semantic-mode 1)								; help out with semantics
(savehist-mode 1)                 ; Save history
(save-place-mode 1)               ; when buffer is closed, save the cursor position
(blink-cursor-mode 1)
;; (variable-pitch-mode t)
(electric-pair-mode t)            ; Close the brackets automatically
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default shell-file-name "/nix/store/3z2any3vsrsi8l52sw3pdhmwyzsqwhs7-system-path/bin/fish")

(setq ad-redefinition-action            'accept
      default-buffer-file-coding-system 'utf-8
      mouse-autoselect-window           t ;; Auto hover mouse
      ;; blink-cursor-interval             0.7       ;; Little slower cursor blinking . default is 0.5
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
      ;;default-directory "~/"
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
      backward-delete-char-untabify-method 'hungry
      xterm-mouse-mode t) ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines)

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

;; (use-package general
;;   :after evil
;;   :config
;;   (general-evil-setup t))

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
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

(global-hl-line-mode t)

;; (use-package indent-guide
;;   :hook (prog-mode . indent-guide-mode)
;;   

;; hl-volatile
;; (use-package volatile-highlights
;;   :diminish
;;   :commands volatile-highlights-mode
;;   :hook (after-init . volatile-highlights-mode)
;;   :custom-face
;;   (vhl/default-face ((nil (:foreground "#FF3333" :background "BlanchedAlmond"))))) ; "#FFCDCD"

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config (which-key-mode))

;;________________________________________________________________
;;    Setup fonts
;;________________________________________________________________
(set-face-attribute 'default t
                    :font "Iosevka" ;; Hack, Input, Terminess, Nerd, Font Propo
                    :height 150
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka"
                    :height 150
                    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height 150
                    :weight 'medium)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Iosevka 15"))
(add-to-list 'default-frame-alist
             '(font . "Iosevka 15"))
(add-to-list 'default-frame-alist '(font . "Iosevka 15"))

;; Changes certain keywords to symbols, such as lamda
(setq global-prettify-symbols-mode t)

(use-package all-the-icons
  ;; :demand t
  ;; :ensure t
  :if (display-graphic-p)
  :config
  ;; Make sure the icon fonts are good to go
  ;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

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

;;________________________________________________________________
;;    Setup theme
;;________________________________________________________________
;; (use-package nano-theme
;;   :straight (nano-theme :type git
;;       			:host github
;;       			:repo "rougier/nano-theme"))

(use-package doom-themes
  :if window-system
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

(use-package gruvbox-theme)
(use-package ef-themes)

(use-package theme-changer
  :ensure t
  :demand t
  :config
  (setq calendar-location-name "Vladivostok, RU")
  (setq calendar-latitude 43.11)
  (setq calendar-longitude 131.88)
  )

(require 'theme-changer)
;; (change-theme 'ef-deuteranopia-light 'ef-deuteranopia-dark)
;; (change-theme 'ef-spring 'ef-dark) ;; ef-trio-light / ef-maris-dark, ef-elea-dark, ef-dark
;; (change-theme 'nano-dark 'nano-dark) ;; ef-trio-light / ef-maris-dark, ef-elea-dark, ef-dark
;; (change-theme 'doom-ayu-light 'doom-ayu-mirage)
(change-theme 'doom-one-light 'doom-one)
;; (load-theme 'doom-one)

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-dark-theme 'doom-one)
  (setq auto-dark-light-theme 'doom-one-light)
  (setq auto-dark-polling-interval-seconds 0)
  (setq auto-dark-allow-osascript nil)
  (setq auto-dark-allow-powershell nil)
  ;; (setq auto-dark-detection-method nil) ;; dangerous to be set manually

  (auto-dark-mode t))

;;________________________________________________________________
;;    Modeline
;;________________________________________________________________
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (display-battery-mode 1)
  (display-time-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

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

;; hl-numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (setq display-line-numbers-width 3))

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

(setq-default tab-width 2)

(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode))
  :custom
  (olivetti-body-width 150)
  :delight " ⊗") ; Ⓐ ⊛

;; Get rid of the background on column views
;; (set-face-attribute 'org-column nil :background nil)
;; (set-face-attribute 'org-column-title nil :background nil)

(use-package org
  :straight (:type built-in)
  :ensure nil
  ;; :defer t
  ;; :after org
  ;; :demand t
  :delight org-mode "✎"
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
  :bind (("C-c l"               . org-store-link)
         ("C-c c"               . org-capture)
       ("C-c f"               . org-footnote-new))
  :config
  (setq
   org-ellipsis " ▾" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
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
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-cycle-separator-lines 2
   org-startup-with-inline-images t
   org-display-remote-inline-images t
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 2
   org-fontify-quote-and-verse-blocks t
   org-export-with-smart-quotes t

   org-checkbox-hierarchical-statistics nil
   org-read-date-prefer-future 'time
   org-agenda-todo-ignore-scheduled 'future
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-todo-ignore-with-date t
   org-image-actual-width '(300))

  ;; Log time a task was set to DONE.
  (setq org-log-done (quote time))

  ;; Don't log the time a task was rescheduled or redeadlined.
  (setq org-log-redeadline t) ; changed
  (setq org-log-reschedule t) ; changed

  (setq org-todo-keyword-faces
        '(
          ("TODO" :background "indian red" :foreground "white" :weight bold)
          ("DOING" :background "tomato" :foreground "white" :weight bold)
          ("NEXT" :background "sky blue" :foreground "black" :weight bold)
          ("WAITING" :background "olive drab" :foreground "black" :weight bold)
          ("STOPPED" :background "firebrick2" :foreground "white" :weight bold)
          ("REVIEW" :background "cyan" :foreground "black" :weight bold)
          ("DONE" :background "pale green" :foreground "black" :weight bold)
          ("ARCHIVED" :background "light slate blue" :foreground "white" :weight bold)
          ("CANCELLED" :background "dark red" :foreground "white" :weight bold)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(d)" "NEXT(n)" "WAITING(w)" "STOPPED(s)" "REVIEW(r)" "|" "DONE(o)" "ARCHIVED(a)" "CANCELLED(c)")))

  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (require 'org-tempo)

    (add-hook 'org-babel-after-execute-hook (lambda ()
                                              (when org-inline-image-overlays
                                                (org-redisplay-inline-images))))
    (add-to-list 'org-modules 'org-tempo t))
  ;; (setq org-modules
  ;; 	'(org-crypt
  ;;         org-bookmark
  ;;         org-eshell
  ;;         org-irc))

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

    (define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today)

    (use-package org-habit-stats
      :config
      (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
      (add-hook 'org-agenda-mode-hook
              (lambda () (define-key org-agenda-mode-map "Z" 'org-habit-stats-view-next-habit-in-agenda)))))

  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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

  ;; ────────────────────────────── Prettify Symbols ─────────────────────────────
  ;; Beautify Org Checkbox Symbol
  (defun ma/org-buffer-setup ()
    "Something for like document, i guess 😕."
    (push '("[ ]" . "☐" ) prettify-symbols-alist)
    (push '("[X]" . "☑" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist))
  (add-hook 'org-mode-hook 'ma/org-buffer-setup)

  (use-package org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq
     ;; Edit settings
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     ;; Appearance
     org-modern-radio-target    '("❰" t "❱")
     org-modern-internal-target '("↪ " t "")
     ;; org-modern-progress t
     ;; org-modern-statistics nil
     org-modern-todo nil
     org-modern-tag t
     org-modern-timestamp t
     org-modern-statistics t
     ;; org-modern-table nil
     org-modern-priority t
     org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
     org-modern-hide-stars " "
     org-modern-keyword "‣")

    (setq org-modern-priority-faces
          (quote ((?A :background "red"
          	    :foreground "black")
          	(?B :background "dark orange"
          	    :foreground "black")
          	(?C :background "tan"
          	    :foreground "black")))))

  ;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element
  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t
          org-appear-autolinks 'just-brackets))

  ;(use-package org-transclusion
  ;  :after org
  ;  :config
  ;  (define-key global-map (kbd "<f12>") #'org-transclusion-add)
  ;  (define-key global-map (kbd "C-c t") #'org-transclusion-mode))

  (use-package org-download
    :demand t
    :bind (:map org-mode-map
    	      ("C-x p m"    . org-download-clipboard)
    	      ("C-x p o"    . org-download-image))
    :config
    (setq-default org-download-image-dir "./assets-org/"))

  (use-package org-analyzer)

  (use-package counsel-jq)

  (use-package tsc)
  (use-package ob-typescript)
  (use-package ob-http)
  (use-package ob-rust)
  (use-package ob-go)
  (use-package ob-solidity)
  (use-package ob-sql-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http       . t)
     (js         . t)
     (solidity   . t)
     (typescript . t)
     (shell      . t)
     (python     . t)
     (rust       . t)
     (go         . t)
     (C          . t)
     (sql        . t))))
;; (latex      . t)

(use-package toc-org
  :config
  (if (require 'toc-org nil t)
      (progn
      (add-hook 'org-mode-hook 'toc-org-mode)

      ;; enable in markdown, too
      ;; (add-hook 'markdown-mode-hook 'toc-org-mode)
      ;; (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
      )
    (warn "toc-org not found")))

(use-package org-reverse-datetree)

;; "%Y"                    ; year
;; (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
;; "%Y-%m-%d %A"           ; date
(setq-default org-reverse-datetree-level-formats
              '("Week №%W {%B-%Y}")) ; week

(setq org-capture-templates
      '(("c" "New task (GTD)" entry (file "~/Org/agenda/GTD/Inbox.org")
         "* TODO %?")

              ("p" "PROGRAMMING (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org"
    							    (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("💻 Programming 👾"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t" :clock-in t)

              ("m" "Meeting" entry (file+function "~/Org/Meetings.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("Meetings"))))
       "* Meeting for %U\nSCHEDULED: %U"
       :clock-in t
         :time-prompt t)

      ("e" "EXAMS (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("📖 Exams 📖"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t")

      ("l" "PERSONAL LIFE (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("💁 Personal life 🤷"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t")
      ;; ("b" "Book for month" entry (file+datetree "~/Org/agenda/ReadAhead.org")
      ;;  "* TODO %?\nSCHEDULED: %t--%t DEADLINE: %t" :tree-type month)
      ))

;; ("c" "Book (paste from clipboard)" entry (file "~/Org/agenda/Reading-list.org")
;;  "%(let* ((url (substring-no-properties (current-kill 0)))
;;                       (details (org-books-get-details url)))
;;                  (when details (apply #'org-books-format 1 details)))")

;; ("l" "Book log" item (function org-books-visit-book-log)
;;  "- %U %?" :prepend t)

(setq org-datetree-add-timestamp t)

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

(use-package org-notifications
  :init
  (setq org-clock-sound "~/.emacs.d/sounds/sound.wav")
  (org-notifications-start))

(use-package org-cliplink
  :demand t
  :config
  (global-set-key (kbd "C-x p i") 'org-cliplink))

(use-package org-pomodoro
  :straight (:host github :repo "marcinkoziej/org-pomodoro" :branch "master")
  :bind (("C-c k"               . org-pomodoro))
  :config
  ;; First of all you sould install aplay or afplay
  (use-package sound-wav
    :demand t) ;; dep for org-pomodoro
  (use-package powershell
    :demand t) ;; dep for org-pomodoro
  (setq org-pomodoro-length 35
      org-pomodoro-short-break-length 5
      org-pomodoro-long-break-length 15
      org-pomodoro-long-break-frequency 4
      org-pomodoro-play-sounds 1

      org-pomodoro-finished-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"
      org-pomodoro-long-break-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"
      org-pomodoro-short-break-sound "/home/berezhnev/.emacs.d/sounds/sound.wav"))

(use-package org-clock-budget
  :straight (:host github :repo "Fuco1/org-clock-budget" :branch "master"))

;; (use-package org-ql)

;; (use-package org-books
;;   :config
;;   (setq org-books-file "~/Org/agenda/Reading-list.org"))

;; (use-package focus
;;   :demand t
;;   :config
;;   ;; '((prog-mode . defun) (text-mode . sentence) (org-mode . sentence))
;;   (focus-mode 1)
;;   (add-to-list 'focus-mode-to-thing '(org-mode . org-element)))

;; (use-package darkroom
;;   :config
;;   (setq darkroom-text-scale-increase 1))

;; (use-package org-anki) ;; https://github.com/eyeinsky/org-anki/
;; (use-package anki-editor)  ;; https://github.com/louietan/anki-editor

;; (use-package calfw
;;   :config
;;   (setq calendar-week-start-day 1)
;;   (setq cfw:org-agenda-schedule-args '(:timestamp))) ; 0:Sunday, 1:Monday

;; (use-package org-timeblock
;;   :straight (org-timeblock :type git
;;     			   :host github
;;     			   :repo "ichernyshovvv/org-timeblock")
;;   :bind
;;   (:map global-map
;;     	("C-c s" . org-timeblock))
;;   :config
;;   (setq org-timeblock-inbox-file "/home/berezhnev/Org/agenda/PlanAhead.org")
;;   (setq org-timeblock-n-days-view 5)
;;   (setq org-timeblock-tag-colors '(("school" " #ffff00" "#000000")
;;     				   ("programming" "#d60000" "#000000")
;;     				   ("exams" "#8e24aa" "#000000")
;;     				   ("languages" "#0b8043" "#000000")
;;     				   ("german" "#0b8043" "#000000")
;;     				   ("ananasya" "#f5511d" "#000000")
;;     				   ("violin" "#33b679" "#000000")
;;     				   ("diary" "#e67c73" "#000000")
;;     				   ("home" "#616161" "#000000")
;;     				   ("books" "#0b8043" "#000000")
;;     				   ("sleep" "#005f8b" "#fff")
;;     				   ("meditation" "#7cfc00" "#00000"))))

;; (setq org-padding-block-begin-line-padding '(2.0 . nil))
;; (setq org-padding-block-end-line-padding '(nil . 1.0))
;; (setq org-padding-heading-padding-alist
;;   '((4.0 . 1.5) (3.0 . 0.5) (3.0 . 0.5) (3.0 . 0.5) (2.5 . 0.5) (2.0 . 0.5) (1.5 . 0.5) (0.5 . 0.5)))

;; (use-package org-timeline
;;   :config
;;   (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;; (use-package org-treescope
;;   :custom
;;   (org-treescope-cyclestates-todo '(nil ("TODO") ("WAITING" "DONE")))
;;   ;; (org-treescope-cyclestates-priority '(nil ("A" "B" "C") ("D")))
;;   :bind
;;   (("C-c M-t" . org-treescope)))

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(use-package org-agenda
  :ensure nil
  :straight (:type built-in)
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config
  (use-package org-super-agenda
    :demand t)

  (setq org-agenda-files
        '("~/Org/agenda/PlanAhead.org"
          "~/Org/agenda/Habits.org"))

  (setq org-default-notes-file "~/Org/agenda/Notes.org")

  ;; Set default column view headings: Task Total-Time Time-Stamp
  ;; (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-agenda-skip-scheduled-if-done nil ; changed
        org-agenda-skip-deadline-if-done nil ; changed
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t ; changed
        org-agenda-start-with-log-mode nil)

  ;; (setq org-agenda-clockreport-parameter-plist
  ;;       (quote (:link t :maxlevel 5 :fileskip t :compact t :narrow 80)))

  (setq org-agenda-deadline-faces
        '((1.0001 . org-warning)              ; due yesterday or before
        	(0.0    . org-upcoming-deadline)))  ; due today or later(setq-default org-icalendar-include-todo t)

  ;; (setq calendar-date-style 'european
  ;;       calendar-mark-holidays-flag t
  ;;       calendar-week-start-day 1)

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

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.1)
    (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-breadcrumbs-separator " ❱ "
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
        													       (search . " %i %-12:c")))

  (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                      																      "\n"
                      																      (org-agenda-format-date-aligned date))))

  (regexp-opt '("Tasks" "Notes"))
  (setq org-refile-targets
              '(("Projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

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

                        (org-agenda-files '("~/Org/agenda/PlanAhead.org"
        																		      "~/Org/agenda/GTD/Inbox.org"))
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
  																      :face (:background "dark slate blue"))
  												       ))))

        	  (alltodo "" ((org-agenda-overriding-header "")
        								       ;; (org-agenda-prefix-format "  %i %-12:c [%e] ") ;// + deadline
        								       (org-agenda-prefix-format "  %?-12t% s")
      									       (org-agenda-entry-text-mode t)
        								       (org-agenda-files '("~/Org/agenda/GTD/Inbox.org")) ;; "~/Org/agenda/GTD/Projects.org"
         								       (org-super-agenda-groups
        									      '((:name "Inbox"
  																       :children t
        													       :and (:tag "inbox" :todo "TODO"))))))

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
        								      ;; (org-agenda-prefix-format '((agenda . "%b ∘ %s %(scheduled)")))
        								      ;; (org-agenda-prefix-format '((agenda . "%-12:c%?-12t% s")))
      									      ;; (org-agenda-prefix-format "  %?-12t% s")
      									      (org-agenda-prefix-format "  %t %s")
    										      (org-agenda-files '("~/Org/agenda/Habits.org"))
    										      (org-super-agenda-groups
    										       '((:name "Everytime habits"
                            			      :and (:tag "habits" :tag "everytime"))
                           (:name "Morning habits"
                            			      :and (:tag "habits" :tag "morning"))
                           (:name "Day habits"
                            			      :and (:tag "habits" :tag "day"))
                           (:name "Evening habits"
                            			      :and (:tag "habits" :tag "evening"))
                           (:name "Sport habits"
                            			      :and (:tag "habits" :tag "sport"))
                           (:name "Challenges"
                            			      :and (:tag "habits" :tag "challenge"))
                           (:discard (:anything))
                           (:discard (:not (:tag "habits")))))))))))

  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(use-package org-roam
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

     ;; ("m" "Meeting" plain (file "~/Org/Templates/Meeting.org")
     ;;  :if-new (file+head "meetings/%<%Y-%m-%d-%H:%M>--meeting-${slug}.org" "#+title: ${title}\n#+filetags: :Meeting:\n#+date: %U\n")
     ;;  :unnarrowed t)

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
     ("m" "Morning diary" plain (file "~/Org/Templates/journal/Morning.org") :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))
     ("q" "Quotetions diary" entry "** Quotation of the day (%U)‎\n\n#+begin_quote\n%^{Quote}\n#+end_quote\n+ Author: *%^{Author of quote}*\n\n* Reflections about this quote" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>-quote.org" "#+title: %U\n\n"))

     ("d" "Default diary" entry "** Default (%U): «%?»‎\n\n" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "** %U\n#+title: %U\n\n"))

     ("e" "Evening diary" plain (file "~/Org/Templates/journal/Evening.org") :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))))
  :config
  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
     '("title" "citekey"  "url" "author-or-editor" "keywords" "file")
     orb-process-file-keyword t)
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
        org-roam-ui-open-on-start nil))

(use-package org-roam-timestamps
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

(use-package deft
  :bind ("<f9>" . deft)
  :config
  (setq deft-extensions '(".org")
      deft-text-mode 'org-mode
      deft-directory "~/Org/Notes"
      deft-recursive nil))

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

(use-package bufferlo
 :ensure t
 :config
 (bufferlo-mode 1))

(use-package helm
  :demand t
  :init
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  :config
  ;;(setq helm-mode 1)
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

(global-set-key (kbd "M-x") 'helm-M-x)

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
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;;(use-package treemacs-magit
;;  :after (treemacs magit)
;;  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

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
        '(file-time file-size collapse subtree-state vc-state git-msg)) ;; all-the-icons 
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (
   ;; ("C-c f" . dirvish-fd)
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

;; (use-package dired-subtree
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("i" . dired-subtree-insert)
;;             (";" . dired-subtree-remove)))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
     )

;; '(global-dired-hide-details-mode t)
(use-package dired
  :ensure nil
  :straight (:type built-in)
  :demand t
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        ;; image-dired-dir (concat doom-cache-dir "image-dired/")
        ;; image-dired-db-file (concat image-dired-dir "db.el")
        ;; image-dired-gallery-dir (concat image-dired-dir "gallery/")
        ;; image-dired-temp-image-file (concat image-dired-dir "temp-image")
        ;; image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :defer t
  :config
  ;; (set-popup-rule! "^\\*image-dired"
  ;;   :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  ;; (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)
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
  (define-key dired-mode-map (kbd "n") 'dired-create-directory)) ; Bind n to create a new directory
  ;; Drag-and-drop to `dired`
  ;; (add-hook 'dired-mode-hook 'org-download-enable)
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
  ;; (use-package dired-open
  ;;   :config
  ;;   ;; Doesn't work as expected!
  ;;   (add-to-list 'dired-open-functions #'dired-open-xdg t))
  (use-package dired-sidebar
    :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
    :ensure t
    :commands (dired-sidebar-toggle-sidebar)
    ;; :init
    ;; (add-hook 'dired-sidebar-mode-hook
    ;;           (lambda ()
    ;;             (unless (file-remote-p default-directory)
    ;;               (auto-revert-mode))))
    :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-subtree-line-prefix "__")
    (setq dired-sidebar-theme 'vscode)
    (setq dired-sidebar-use-term-integration t)
    ;;(setq dired-sidebar-use-custom-font t))
  (use-package dired-single))

;; Sort dirs first
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program t)

(provide 'dired-setting)

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
  (global-set-key (kbd "C-c C-f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

(global-set-key (kbd "\C-c C-f") 'format-all-buffer)

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
  (setq dashboard-items '((recents  . 8)
                          ;; (agenda   . 5)
                          (projects . 7)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

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

(use-package flycheck
    :hook (prog-mode . flycheck-mode)
    :diminish
    :custom
    (flycheck-indication-mode 'left-fringe)
    (flycheck-display-errors-delay 0.5)
    (flycheck-check-syntax-automatically '(save idle-change))
    (flycheck-idle-change-delay 0.5)
    ;; :config
    ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;     (add-hook 'org-mode-hook 'flyspell-mode)
)

  (use-package flycheck-inline
    :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :straight t
  :hook ((go-mode         . lsp)
         (rust-mode       . lsp)
  			       (emacs-lisp-mode . lsp)
  			       (lisp            . lsp)
         (js-mode         . lsp)
  			       (solidity-mode   . lsp)
         (typescript-mode . lsp)
         ;; (LaTeX-mode      . lsp)
         (lsp-mode        . lsp-enable-which-key-integration))
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
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (delete 'lsp-terraform lsp-client-packages)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Symbol highlighting
(setq lsp-enable-symbol-highlighting nil)

;; Need for LSP
;; (use-package sideline
;;   :init
;;   (setq sideline-backends-skip-current-line t  ; don't display on current line
;;         sideline-order-left 'down              ; or 'up
;;         sideline-order-right 'up               ; or 'down
;;         sideline-format-left "%s   "           ; format for left aligment
;;         sideline-format-right "   %s"          ; format for right aligment
;;         sideline-priority 100                  ; overlays' priority
;;         sideline-display-backend-name t))      ; display the backend name

;; (use-package sideline-flycheck
;; :hook (flycheck-mode . sideline-flycheck-setup))

;; (setq tab-always-indent 'complete)
;; (setq completion-cycle-threshold 3)

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

(use-package lsp-ui
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
;; lsp-ui-sideline-delay 0.02
;; lsp-ui-sideline-ignore-duplicate t
;; lsp-ui-sideline-show-code-actions t
;; lsp-ui-sideline-update-mode 'line
;; lsp-ui-sideline-show-diagnostics t
;; lsp-ui-sideline-show-hover t
;; lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default
;; lsp-ui-sideline-mode nil

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package import-js)
;; sudo npm install import-js -g

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
;; (use-package tsi
;;   :after tree-sitter
;;   :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (apheleia-global-mode +1))

(use-package tide
  :after (company flycheck)
  :hook ((typescript-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
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
(add-hook 'tsx-ts-mode-hook 'setup-tide-mode)
(add-hook 'js2-mode-hook 'setup-tide-mode)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook 'setup-tide-mode)

(use-package json-mode
  :defer 20
  :custom
  (json-reformat:indent-width 2)
  :mode (("\\.bowerrc$"     . json-mode)
       ("\\.jshintrc$"    . json-mode)
       ("\\.json_schema$" . json-mode)
       ("\\.json\\'" . json-mode))
  :bind (:package json-mode-map
		:map json-mode-map
		("C-c <tab>" . json-mode-beautify)))

(use-package rust-playground)

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

(use-package rustic
  :ensure
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
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package solidity-mode)
(use-package solidity-flycheck)
(use-package company-solidity)

(use-package go-mode)
;; (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t ; aligns annotation to the right hand side
        company-require-match 'never
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :custom
  (company-idle-delay 0.2) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
      ("C-n". company-select-next)
      ("C-p". company-select-previous)
      ("M-<". company-select-first)
      ("M->". company-select-last)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 30))
