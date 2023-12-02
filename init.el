;; emacsclient --no-wait--alternate-editor=emacs [FILE]
;; (require 'server)
;; (server-start)

;;________________________________________________________________
;;    Install straight
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

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq vc-follow-symlinks t)
(setq straight-pull-recipe-repositories t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;________________________________________________________________
;;    Install use-package (straight integration)
;;________________________________________________________________
;; Install `use-package'.
(straight-use-package 'use-package)

;; Install packages in `use-package' forms with `straight'. (not the built-in
;; package.el)
(setq straight-use-package-by-default t)

;; Key Chord functionality in use-package
(use-package use-package-chords
  :config (key-chord-mode 1))

;; Diminish functionality
;; (use-package diminish)

;; Configure use-package
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)  ; :ensure t by default
  (use-package-always-defer nil) ; :defer t by default
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
;;    Setup config using org-mode
;;________________________________________________________________
;; (org-babel-load-file
;;  (expand-file-name
;;   "README.org"
;;   user-emacs-directory))

(use-package bug-hunter)

;;________________________________________________________________
;;    Base settings of Emacs
;;________________________________________________________________
(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

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
(electric-pair-mode t)            ; Close the brackets automatically
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

(use-package russian-holidays
  :config
  (setq calendar-holidays russian-holidays))

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

(xterm-mouse-mode t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; ;;      SPELLING       ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.5)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 0.5)
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package ispell
  ;; :bind (:map global-map
  ;; 	 ("<f8>" . ispell-word)) ; easy spell check
  :custom
  (ispell-program-name "hunspell") ; require Hunspell
  (ispell-dictionary "ru_RU")
  ;; (ispell-personal-dictionary "~/.emacs.d/.hunspell_personal")
  ;; :config
  (setq ispell-local-dictionary "ru")
  (setenv "LANG" "ru_RU.UTF-8")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;"        . nil)
              ("C-,"        . nil)
              ("C-."        . nil)
              ("M-TAB"      . nil)
              ("C-x M-$"    . flyspell-buffer)
              ("C-<f7>"     . flyspell-auto-correct-word)
              ("C-<f12>"    . flyspell-auto-correct-previous-word))
  :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell)))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :delight " ⓢ")

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
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

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
  (helm-mode 1)
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

;;________________________________________________________________
;;    Dashboard
;;________________________________________________________________
(use-package dashboard
  ;; :straight (:build t)
  :after all-the-icons
  :ensure t
  :defer nil
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
                          ;; (agenda   . 5)
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
  :mode (("\\.pdf\\'" . pdf-view-mode))
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
;; (defhydra hydra-pdftools (:color blue :hint nil)
;;   "
;;                                                                       ╭───────────┐
;;        Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
;;    ╭──────────────────────────────────────────────────────────────────┴───────────╯
;;          ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
;;          ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
;;          ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
;;          ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
;;     _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
;;          ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
;;          ^^_n_^^      ^ ^  _r_eset slice box
;;          ^^^↓^^^
;;          ^^_G_^^
;;    --------------------------------------------------------------------------------
;;         "
;;   ("\\" hydra-master/body "back")
;;   ("<ESC>" nil "quit")
;;   ("al" pdf-annot-list-annotations)
;;   ("ad" pdf-annot-delete)
;;   ("aa" pdf-annot-attachment-dired)
;;   ("am" pdf-annot-add-markup-annotation)
;;   ("at" pdf-annot-add-text-annotation)
;;   ("y"  pdf-view-kill-ring-save)
;;   ("+" pdf-view-enlarge :color red)
;;   ("-" pdf-view-shrink :color red)
;;   ("0" pdf-view-scale-reset)
;;   ("H" pdf-view-fit-height-to-window)
;;   ("W" pdf-view-fit-width-to-window)
;;   ("P" pdf-view-fit-page-to-window)
;;   ("n" pdf-view-next-page-command :color red)
;;   ("p" pdf-view-previous-page-command :color red)
;;   ("d" pdf-view-dark-minor-mode)
;;   ("b" pdf-view-set-slice-from-bounding-box)
;;   ("r" pdf-view-reset-slice)
;;   ("g" pdf-view-first-page)
;;   ("G" pdf-view-last-page)
;;   ("e" pdf-view-goto-page)
;;   ("o" pdf-outline)
;;   ("s" pdf-occur)
;;   ("i" pdf-misc-display-metadata)
;;   ("u" pdf-view-revert-buffer)
;;   ("F" pdf-links-action-perfom)
;;   ("f" pdf-links-isearch-link)
;;   ("B" pdf-history-backward :color red)
;;   ("N" pdf-history-forward :color red)
;;   ("l" image-forward-hscroll :color red)
;;   ("h" image-backward-hscroll :color red))

;; (use-package emacs-prisma-mode
;;   :straight (:host github :repo "pimeys/emacs-prisma-mode" :branch "master"))

(require 'plstore)
(setq plstore-cache-passphrase-for-symmetric-encryption t)

(add-to-list 'plstore-encrypt-to '("1E26C975819E142DA3C5C5756CD99B7103102AAC"))
    
(setq org-gcal-client-id "608889424823-1ieosumpjohasojr85069r5i0235dre7.apps.googleusercontent.com"
      org-gcal-client-secret "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"
      org-gcal-fetch-file-alist '(("vova21473@gmail.com" .  "/home/chopin/Org/agenda/gCal.org")))

(use-package org-gcal
  :config
  (global-set-key (kbd "C-c j") 'org-gcal-sync)
  (global-set-key (kbd "C-c h") 'org-gcal-post-at-point))

(run-at-time "08:00" nil 'org-gcal-sync)
(run-at-time "07:30" nil 'org-gcal-sync)
(run-at-time "20:30" nil 'org-gcal-sync)
(run-at-time "20:45" nil 'org-gcal-sync)


;;;; Load custom-files
(defun load-directory (dir)
  "Load all *.el files in a directory."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/config") ; load my configuration of packages

;; (use-package org-reveal
;;   :ensure nil
;;   :straight (:host github :repo "yjwen/org-reveal" :branch "master"))
;; (require 'ox-reveal)

(use-package ox-reveal)
    
(use-package ledger-mode
  ;; :init
  ;; (setq ledger-clear-whole-transactions 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode))
(use-package sudoku)
(use-package sudo-save)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(org-gcal-auto-archive nil)
 '(org-gcal-managed-update-existing-mode "gcal")
 '(org-gcal-recurring-events-mode 'nested)
 '(org-gcal-remove-events-with-cancelled-todo t)
 '(org-gcal-up-days 90)
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:foreground "bisque4" :slant italic)))))
