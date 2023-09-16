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

;;________________________________________________________________
;;    Setup fonts
;;________________________________________________________________

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Load config
;; (load-directory "~/.emacs.d/config")

(add-to-list 'load-path "~/.emacs.d/config/")
(require 'appereance-setting)
(require 'dired-setting)
(require 'flycheck-setting)
(require 'formatter-setting)
(require 'git-setting)
(require 'keymap-setting)
(require 'lsp-setting)
(require 'org-roam-setting)
(require 'org-setting)
(require 'pdf-viewer-setting)
(require 'socials-setting)
(require 'spell-setting)
(require 'ui-setting)

(use-package ox-hugo
  ;;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :config
  (use-package org-re-reveal  )
  (use-package ox-reveal  )
  (setq org-reveal-root "file:~/Org/Presentations/reveal.js/"))

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

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://nplus1.ru/rss" "https://naked-science.ru/article/category/physics/feed" "https://nuancesprog.ru/feed/" "https://dev.to/rss" "https://elementy.ru/rss/news" "https://postnauka.org/feed" "https://www.sciencedaily.com/rss/top/science.xml" "https://www.sciencedaily.com/rss/top/technology.xml" "https://www.sciencedaily.com/rss/space_time.xml" "http://www.sciencedaily.com/rss/computers_math.xml" "https://habr.com/ru/rss/feed/articles/bd769e8234cb6e6444ae3197fd0c0d9b/?fl=ru" "https://www.reddit.com/r/rust/.rss" "https://www.privacytools.io/guides/rss.xml" "https://stallman.org/rss/rss.xml" "http://thenewstack.io/blog/feed/" "https://habr.com/en/rss/flows/develop/articles/?fl=en" "https://thenewstack.io/frontend-development/feed/" "https://thenewstack.io/devops/feed"))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(nano-modeline nano-theme nano-agenda ssh ob-sql-mode dirvish bug-hunter focus org-recur org-agenda-property zygospore which-key web-mode volatile-highlights use-package typescript-mode treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons tree-sitter-langs tide theme-changer telega solaire-mode sideline-flycheck saveplace-pdf-view rust-playground rust-mode reverse-im rainbow-delimiters prettier-js pbcopy parrot ox-reveal ox-hugo org-transclusion org-super-agenda org-roam-ui org-roam-timestamps org-roam-bibtex org-ref org-re-reveal org-noter-pdftools org-modern org-download org-cliplink org-caldav org-appear org-alert olivetti ob-typescript ob-rust multi-vterm minions magit-todos lsp-ui ligature kind-icon json-mode js2-mode indent-guide import-js highlight-numbers highlight-indent-guides helm-lsp helm-bibtex gruvbox-theme go-mode git-gutter-fringe general fzf format-all flycheck-rust flycheck-inline fancy-battery evil-collection emojify ement elfeed doom-themes doom-modeline djvu dired-single dired-sidebar dired-rainbow dired-open dashboard dap-mode corfu company-org-block company-box company-bibtex company-auctex citar-org-roam citar-embark cargo blamer beacon apheleia ac-math)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modern-horizontal-rule ((t (:inherit org-hide :strike-through "light sky blue")))))
