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

(xterm-mouse-mode t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package no-littering
  :ensure t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq kill-buffer-query-functions nil)

(add-hook 'auto-package-update-after-hook
          (lambda ()
            (load-library "yasnippet-snippets.el")))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(defun frostyx/set-default-font (size)
  (set-face-attribute
   'default nil
   :family "Iosevka"
   ;;:foundry "ADBO"
   :height 150 ;;size
   :weight 'regular ;;'normal
   :width 'normal
   :slant 'normal))

(frostyx/set-default-font 90)

(use-package doom-themes
  :if window-system
  :ensure t
  :defer nil
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

(load-theme 'doom-one)

(use-package ob-async
        :ensure t)

(use-package general
  :ensure t)

(general-evil-setup)

(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  ;; (setq evil-vsplit-window-right t)
  ;; (setq evil-split-window-below t)
  ;;(setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)

  (setq evil-ex-set-initial-state 'normal)

  :config ;; tweak evil after loading it
  (evil-mode))

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) (kbd "<up>") nil)
    (define-key (eval map) (kbd "<down>") nil)
    (define-key (eval map) (kbd "<left>") nil)
    (define-key (eval map) (kbd "<right>") nil)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package evil-terminal-cursor-changer
	:ensure t
	:config
	(unless (display-graphic-p)
          (require 'evil-terminal-cursor-changer)
          (evil-terminal-cursor-changer-activate))
	 (setq evil-motion-state-cursor 'box)  ; █
     (setq evil-visual-state-cursor 'box)  ; █
     (setq evil-normal-state-cursor 'box)  ; █
     (setq evil-insert-state-cursor 'bar)  ; ⎸
     (setq evil-emacs-state-cursor  'hbar)) ; _

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'org-super-agenda-mode 'emacs)
  (evil-set-initial-state 'calibredb-mode 'normal)
  (evil-set-initial-state 'enlight-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-mode 'emacs)
  (evil-set-initial-state 'org-timeblock-list-mode 'emacs)
  ;; (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'treemacs-mode 'emacs)
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-set-initial-state 'sunrise-mode 'emacs)
  (evil-collection-init))

(use-package magit
	:ensure t)

(evil-leader/set-key
  "ga" 'magit-stage-file
  "gc" 'magit-commit  ;; Maybe magit-commit-create
  "gp" 'magit-push-current) ;; @TODO still asks for something, use more specific function

(evil-leader/set-key
  "w" 'evil-window-vsplit)

(general-define-key
 :states '(normal visual insert emacs motion)
 "C-x l" 'evil-switch-to-windows-last-buffer)

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package git-gutter
	:ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
	:ensure t
	:config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package reverse-im
  ;; :ensure t ; install `reverse-im' using package.el
  :quelpa (reverse-im
           :fetcher github
           :repo "emacsmirror/reverse-im")
  :demand t
  ;;:after char-fold ; but only after `char-fold' is loaded
  :custom
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  (reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode t)) ; turn the mode on

(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
	:config
	(setq doom-modeline-height 22)
		doom-modeline-icon nil
		doom-modeline-buffer-file-name-style 'buffer-name
		doom-modeline-buffer-encoding nil
		doom-modeline-env-version nil
		doom-modeline-mode-alist '())

(use-package hide-mode-line
  :ensure t
  :config
  (evil-leader/set-key
    "h" 'global-hide-mode-line-mode))

(use-package olivetti
  :ensure t
  :defer t
  :hook ((text-mode         . olivetti-mode)
         ;; (org-agenda-mode   . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)))
(setq olivetti-body-width 150)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-fuzzy-match t)


  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  ; (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  ; http://cachestocaches.com/2016/12/vim-within-emacs-anecdotal-guide/
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)

  (define-key helm-map [escape] 'helm-keyboard-quit))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package rainbow-mode
  ;; There is a bug visualizing even #def in .Xdefaults
  :ensure t
  :config
  nil)

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq fci-rule-width 1)
;;   (setq fci-rule-color my/gray))

(setq tab-width 2)

(defun hook-tab-width ()
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (setq python-indent-offset 2))
(add-hook 'prog-mode-hook #'hook-tab-width)

(setq evil-shift-width 2)


(use-package company
    :ensure t
    :config
	(setq company-idle-delay 0)
	(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode)

  :config
  (setq company-box-icon-right-margin 1))

(setq company-frontends '(company-tng-frontend company-box-frontend))

(use-package company-org-block
  :ensure t
  :defer t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package diff-hl
  :ensure t)

(use-package flycheck
  :ensure t)

(setq flycheck-highlighting-mode 'lines)


(use-package org
  :straight (:type built-in)
  :bind (("C-c C-x C-j" . org-clock-goto))
  :ensure nil
  :bind (("C-c l"               . org-store-link)
         ("C-c f"               . org-footnote-new)))

(define-key global-map (kbd "C-c u") #'calendar)

(setq-default org-reverse-datetree-level-formats
              '("Week №%W {%B-%Y}"))

(setq org-capture-templates
      '(("c" "New task" entry (file "~/Org/agenda/GTD/Inbox.org")
         "* TODO %?")

        ("p" "PROGRAMMING (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org"
                                                              (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("💻 Programming 👾"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t" :clock-in t)

        ("m" "Meeting" entry (file+function "~/Org/Meetings.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("Meetings"))))
         "* Meeting for %U\nSCHEDULED: %U"
         :clock-in t
         :time-prompt t)

        ("e" "EXAMS (week's task)" entry (file+function "~/Org/agenda/PlanAhead.org" (lambda () (org-reverse-datetree-goto-date-in-file nil :olp '("📖 Exams 📖"))))
         "* TODO %?\nSCHEDULED: %t DEADLINE: %t")))

(setq org-datetree-add-timestamp t)

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(setq
 org-ellipsis " ⤵" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
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
 ;; org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-cycle-separator-lines 2
 org-startup-with-inline-images t
 org-display-remote-inline-images t
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 2
 ;; org-fontify-quote-and-verse-blocks t
 org-export-with-smart-quotes t

 org-checkbox-hierarchical-statistics nil
 org-read-date-prefer-future 'time
 org-agenda-todo-ignore-scheduled 'future
 org-agenda-tags-todo-honor-ignore-options t
 org-agenda-todo-ignore-with-date t
 org-image-actual-width '(300)
 org-log-done (quote time)
 ;; Don't log the time a task was rescheduled or redeadlined.
 org-log-redeadline t ; changed
 org-log-reschedule t)

(with-eval-after-load 'org
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-modules 'org-tempo t))

(evil-leader/set-key
        "z" '(org-agenda nil "z"))

(global-set-key (kbd "C-c C-x o") 'org-clock-out)
(global-set-key (kbd "C-c C-x j") 'org-clock-go-to)

(use-package org-super-agenda
	:ensure t)

(use-package org-agenda
  :ensure nil
  :straight (:type built-in)
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config

  (setq org-agenda-start-on-weekday 0
        org-agenda-skip-scheduled-if-done t ; changed
        org-agenda-skip-deadline-if-done t ; changed
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t ; changed
        org-agenda-start-with-log-mode nil
    		org-agenda-deadline-faces
        '((1.0001 . org-warning)              ; due yesterday or before
          (0.0    . org-upcoming-deadline))   ; due today or later
    		org-icalendar-combined-name "Hugo Org"
    		org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
    		org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
    		org-icalendar-timezone "Asia/Vladivostok"
    		org-icalendar-store-UID t
    		org-icalendar-alarm-time 30
    		calendar-date-style 'european
    		calendar-week-start-day 0
        calendar-mark-holidays-flag t
        calendar-mark-diary-entries-flag nil
  			;; (setq-default org-icalendar-include-todo t)
    		org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((today require-timed remove-match)
                               (500 800 1000 1200 1400 1600 1800 2000)
                               ":  " "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format
  			'((agenda . "%-10c | %?-12t% s")
  				(todo . "%-10s")
  				(tags . "%t %-10c | %s")
  				(search . "%c %t %s"))
        org-agenda-clockreport-parameter-plist
        (quote (:maxlevel 5 :compact t :wstart 0 :link nil :formula % :tags nil :properties ("CATEGORY" "EFFORT") :narrow 80 :fileskip0 t))
        org-agenda-scheduled-leaders '("[S]:" "[S] x%3dd.:")
        org-agenda-deadline-leaders '("[D]:" "[D] +%3dd.:" "[D] -%3dd.:")
    		org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date)))
    		org-default-notes-file "~/Org/agenda/Notes.org"
    		org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")) ;; "~/Org/agenda/Calendar.org"

  (setq mixed-pitch-fixed-pitch-faces
				(quote (line-number-current-line line-number font-lock-comment-face org-done org-todo org-todo-keyword-outd org-todo-keyword-kill org-todo-keyword-wait org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-todo org-tag org-ref-cite-face org-property-value org-special-keyword org-date diff-added org-drawer diff-context diff-file-header diff-function diff-header diff-hunk-header diff-removed font-latex-math-face font-latex-sedate-face font-latex-warning-face font-latex-sectioning-5-face font-lock-builtin-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-doc-face font-lock-function-name-face font-lock-keyword-face font-lock-negation-char-face font-lock-preprocessor-face font-lock-regexp-grouping-backslash font-lock-regexp-grouping-construct font-lock-string-face font-lock-type-face font-lock-variable-name-face markdown-code-face markdown-gfm-checkbox-face markdown-inline-code-face markdown-language-info-face markdown-language-keyword-face markdown-math-face message-header-name message-header-to message-header-cc message-header-newsgroups message-header-xheader message-header-subject message-header-other mu4e-header-key-face mu4e-header-value-face mu4e-link-face mu4e-contact-face mu4e-compose-separator-face mu4e-compose-header-face org-block org-block-begin-line org-block-end-line org-document-info-keyword org-code org-indent org-latex-and-related org-checkbox org-formula org-meta-line org-table org-verbatim)))

	;; Hide duplicates of the same todo item
	;; If it has more than one of timestamp, scheduled,
	;; or deadline information
  (setq org-agenda-skip-timestamp-if-done t
				org-agenda-skip-deadline-if-done t
				org-agenda-skip-scheduled-if-done t
				org-agenda-skip-scheduled-if-deadline-is-shown t
				org-agenda-skip-timestamp-if-deadline-is-shown t)

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.3)
    (set-face-attribute 'org-agenda-date-today nil :height 1.3 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.3))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-custom-commands
        '(("c" "Getting Things Done (GTD)"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-skip-deadline-if-done nil)
                        (org-agenda-clockreport-mode t)
                        (org-agenda-remove-tags t)
                        (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
                        (org-time-budgets-in-agenda-maybe)
                        (org-agenda-include-deadlines t)

                        (org-agenda-files '("~/Org/agenda/PlanAhead.org" "~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Schedule"
    															:time-grid t)
                           (:name "Today"
    															:scheduled today
    															:face (:background "medium sea green" :foreground "white")
                                  :face 'warning)
                           (:name "Future deadline"
    															:deadline future
    															:face (:background "deep sky blue"))
                           (:name "Deadline today"
    															:deadline today
    															:face (:background "black" :foreground "white"))
                           (:name "Passed deadline"
    															:deadline past
                                  :scheduled past
    															:face (:background "salmon"))))))

            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Everytime"
    															:tag ("everytime"))
    											 (:name "Morning"
    													  	:tag ("morning"))
    											 (:name "Day"
    													  	:tag ("day"))
    											 (:name "Evening"
    													  	:tag ("evening"))
    											 ;; (:name "Challenges"
    											 ;;  			:tag "challenge")
    											 (:discard (:anything))
    											 (:discard (:not (:tag "habit")))))))))
          ("p" "Private counter"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "===== Other ====="
    												      :tag "other"
                                  :face (:background "red" :foreground "white" :weight "bold"))
    											 (:discard (:anything))
    											 (:discard (:not (:tag "habit")))))))))

          ("d" "Day results"
					 ((agenda ""
										((org-agenda-span 'day)
										 (org-agenda-overriding-header "\n === TIME REPORT ===")
										 (org-agenda-skip-scheduled-if-done nil)
										 (org-log-done 'time)
										 (org-log-into-drawer t)
										 (org-agenda-skip-deadline-if-done nil)
										 (org-agenda-clockreport-mode t)
										 (org-agenda-remove-tags t)
										 (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
										 (org-time-budgets-in-agenda-maybe)
										 (org-agenda-include-deadlines t)
										 (org-agenda-clockreport-parameter-plist
											'(:scope ("~/Org/agenda/GTD/org-gtd-tasks.org"
																"~/Org/agenda/GTD/gtd_archive_2025"
																"~/Org/agenda/GTD/gtd_archive_2024"
																"~/Org/agenda/GTD/org-gtd-tasks.org_archive")
															 :maxlevel 5
															 :emphasize t
															 :block day
															 :compact t
															 :wstart 0
															 :link nil
															 :formula %
															 :tags nil
															 :properties ("CATEGORY" "EFFORT")
															 :fileskip0 t))
										 (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
																				 "~/Org/agenda/GTD/gtd_archive_2025"
																				 "~/Org/agenda/GTD/gtd_archive_2024"
																				 "~/Org/agenda/GTD/org-gtd-tasks.org_archive"))
										 (org-super-agenda-groups '((:discard (:anything))))))

						(tags "CLOSED>=\"<today>\""
									((org-agenda-overriding-header "\n === COMPLETED TASKS ===")))

						(tags "+STYLE=\"habit\"+CLOSED>=\"<today>\""
									((org-agenda-overriding-header "\n === COMPLETED HABITS ===")))))
					))

  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(use-package ob-typescript
	:ensure t)
(use-package ob-rust
	:ensure t)
(use-package ob-solidity
	:ensure t)
(use-package ob-sql-mode
	:ensure t)
(use-package ob-restclient
  :ensure t)
(use-package gnuplot
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js         . t)
   (solidity   . t)
   (typescript . t)
   (shell      . t)
   (python     . t)
   (rust       . t)
   (C          . t)
   (sql        . t)
   (latex      . t)
   (restclient . t)
   (gnuplot    . t)))

(setq org-tag-alist
        '(
  				("@article" . ?a)
          ("@personal" . ?P)
          ("@coding" . ?p)
          ("@mathematics" . ?m)
  				("@school" . ?s)
          ("@english" . ?e)
  				("@work" . ?w)
          ("@zettelkasten" . ?z)
  				("@idea" . ?i)))

  (use-package org-modern
    :hook (org-mode . org-modern-mode)
    :ensure t
    :config
    (setq
     ;; Edit settings
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     ;; Appearance
     org-modern-radio-target    '("❰" t "❱")
     org-modern-internal-target '("↪ " t "")
     org-modern-block-name
  	 '((t . t)
  	   ("src" "ϰ" "ϰ"))
     org-modern-progress t
     org-modern-statistics nil
     org-modern-todo t
     org-modern-todo-faces (quote (("TODO" :background "indian red" :foreground "white" :weight bold)
  																 ("NEXT" :background "sky blue" :foreground "black" :weight bold)
  																 ("WAIT" :background "olive drab" :foreground "black" :weight bold)
  																 ("DONE" :background "pale green" :foreground "black" :weight bold)
  																 ("CNCL" :background "dark red" :foreground "white" :weight bold)))
     org-modern-priority t
     org-modern-priority-faces (quote ((?A :background "red"
  																				 :foreground "black")
  																		 (?B :background "dark orange"
  																				 :foreground "black")
  																		 (?C :background "tan"
  																				 :foreground "black")))
     org-modern-tag t
     org-modern-timestamp nil
     org-modern-statistics t
     ;; org-modern-table t
     org-modern-tag-faces (quote (("@coding" :background "#d60000" :foreground "#000000")
  																("@personal" :background "#e67c73" :foreground "#000000")
  																("@article" :background "#0b8043" :foreground "#000000")
  																("@mathematics" :background "#bc8f8f" :foreground "#000000")
                                  ("blockchain" :background "#f5511d" "#000000")
  																("solana" :background "#DC1FFF" :foreground "#000000")
  																("rust" :background "#CE412B" :foreground "#000000")
  																("go" :background "#00bfff" :foreground "#00000")
  																("exams" :background "#8e24aa" :foreground "#000000")))
     org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
     org-modern-hide-stars " "
     org-modern-keyword "‣"
     org-modern-table t))
(global-org-modern-mode t)

(use-package org-habit
  :after org
  :ensure nil
  :straight (:type built-in)
  :init
  ;;(add-to-list 'org-modules 'org-habit)
  (progn
    (custom-set-faces
     '(org-habit-clear-face
    	 ((t (:background "pale green"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))

     '(org-habit-clear-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-alert-future-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-alert-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-overdue-face
    	 ((t (:background "light coral"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-overdue-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-ready-face
    	 ((t (:background "pale green"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     '(org-habit-ready-future-face
    	 ((t (:background "gray"
    										:foreground "white"
    										:width expanded
    										:height 1.0
    										:box (:line-width (1 . 1) :color "white")))))
     ))
  :config
  (load "~/.emacs.d/lisp/my-org-habit")
  (setq org-habit-following-days 1
    		org-habit-preceding-days 7
    		org-habit-show-habits nil
    		org-habit-show-all-today t
    		org-habit-graph-column 67
    		org-habit-overdue-glyph ?○
    		org-habit-alert-glyph ?○
    		org-habit-today-glyph ?○
    		org-habit-completed-glyph ?●
              org-habit-ready-future-glyph ?⬡
    		org-habit-show-done-always-green t)

  (defun toggle-org-habit-show-all-today ()
    "Toggle the value of `org-habit-show-all-today' between t and nil."
    (interactive)
    (setq org-habit-show-all-today (not org-habit-show-all-today))
    (message "org-habit-show-all-today is now %s"
    				 (if org-habit-show-all-today "nil" "t"))
    (org-agenda-refresh))

  (define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today))

(use-package org-habit-stats
	:ensure nil
	:load-path "~/.emacs.d/lisp/"
	:config
	(add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
	(add-hook 'org-agenda-mode-hook
						(lambda () (define-key org-agenda-mode-map "Z" 'org-habit-stats-view-next-habit-in-agenda))))

(defun org-habit-count-last-streak (state-str)
  "Подсчитать количество последовательных выполненных дней (●), включая незавершенные задачи (◎).
Стрик включает ◎ только если перед ним есть выполненные дни."
  (let ((streak 0)
        (length (length state-str))
        (has-completed nil))  ; Отслеживаем, были ли выполненные дни
    ;; Идем с конца строки
    (catch 'break
      (dotimes (i length)
        (let ((current-char (aref state-str (- length i 1))))
          (cond
           ;; Незавершенная задача на сегодня (◎) учитывается только при наличии выполненных дней
           ((char-equal current-char ?◎)
            (when has-completed
              (setq streak (1+ streak))))
           ;; Выполненная задача (●) увеличивает стрик и отмечает наличие выполнений
           ((char-equal current-char ?●)
            (setq streak (1+ streak))
            (setq has-completed t))
           ;; Пропущенная задача (○) прерывает подсчет
           (t
            (throw 'break streak))))))
    streak))

(defun org-habit-streak-count ()
  "Display current streak for each habit in org-agenda.
A streak consists of consecutive completed days (●) and can include
today's unfinished tasks (◎) only if there are completed days before it."
  (goto-char (point-min))
  (while (not (eobp))
    (when (get-text-property (point) 'org-habit-p)
      (let ((streak 0))
        ;; Look for the habit's state string (○●◎)
        (save-excursion
          (when (re-search-forward "\\([○●◎]\\)+" (line-end-position) t)
            (let ((state-str (match-string 0)))
              (setq streak (org-habit-count-last-streak state-str)))))
        
        (end-of-line)
        (insert (format " [🔥 %d]" streak))))
    (forward-line 1)))

(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

(defun my-find-work-habit ()
  "Находит привычку '3+ часа работы' в org-файлах и возвращает её данные."
  (let ((work-habit-data nil))
    ;; Перебираем все org-файлы из org-agenda-files
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-point-at 1
          ;; Ищем нашу привычку
          (while (and (not work-habit-data)
                     (re-search-forward "⚡ - 3\\+ часа работы" nil t))
            ;; Сохраняем позицию
            (let ((pos (point)))
              ;; Переходим к началу заголовка
              (org-back-to-heading t)
              ;; Проверяем, является ли это привычкой
              (when (org-is-habit-p)
                ;; Получаем данные привычки
                (setq work-habit-data 
                      (org-habit-stats-parse-todo (point))))
              ;; Возвращаемся к исходной позиции
              (goto-char pos))))))
    work-habit-data))

(defun my-display-work-habit-calendar ()
    "Отображает календарь для привычки '3+ часа работы' в начале org-agenda буфера."
    (let ((work-habit-data (my-find-work-habit)))
      (when work-habit-data
        ;; Создаем календарь в отдельном буфере
        (org-habit-stats-make-calendar-buffer work-habit-data)
        
        ;; Сохраняем текущую позицию в agenda буфере
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "Everytime" nil t)
            (forward-line -1)
            
            ;; Добавляем заголовок
            (insert "\nКалендарь рабочих часов (3+ часа в день)\n")
            (insert "================================\n")
            
            ;; Копируем содержимое календаря из временного буфера
            (let ((calendar-content (with-current-buffer org-habit-stats-calendar-buffer
                                    (buffer-string)))
                  (calendar-overlays (org-habit-stats-get-calendar-overlays)))
              ;; Вставляем содержимое календаря
              (let ((start-pos (point)))
                (insert calendar-content)
                ;; Применяем оверлеи с правильным смещением
                (org-habit-stats-apply-overlays calendar-overlays
                                              (- start-pos 1)
                                              (current-buffer))))
            
            (insert "\n================================\n\n"))))))

  ;; Регистрируем функцию как хук финализации agenda
  (add-hook 'org-agenda-finalize-hook 'my-display-work-habit-calendar)

(load "~/.emacs.d/lisp/gamifications/quest-system-core")
(load "~/.emacs.d/lisp/gamifications/market")
(load "~/.emacs.d/lisp/gamifications/habit-quest")
(load "~/.emacs.d/lisp/gamifications/tasks-quest")

(defun my/insert-daily-reports ()
  "Вставить отчеты habits и day results в текущий буфер с измененными параметрами отображения."
  (interactive)
  (let ((original-habit-column org-habit-graph-column))
    (setq org-habit-graph-column 38)
    (let* ((habits-report (save-window-excursion
                           (with-temp-buffer
                             (org-agenda nil "x")
                             (buffer-string))))
           (day-results (save-window-excursion
                         (with-temp-buffer 
                           (org-agenda nil "d")
                           (buffer-string))))
           ;; Удаляем разделители из обоих отчетов
           (habits-clean (replace-regexp-in-string "^─+\n" "" habits-report))
           (day-clean (replace-regexp-in-string "^─+\n" "" day-results)))
      (setq org-habit-graph-column original-habit-column)
      (insert "** Habits report" habits-clean "\n\n** Day results" day-clean))))
  
(use-package sound-wav
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package powershell
  :ensure t
  :demand t) ;; dep for org-pomodoro

(use-package org-pomodoro
  :ensure t
	:bind (("C-c k" . my/org-pomodoro))
	:config
	(setq org-pomodoro-audio-player (or (executable-find "aplay") (executable-find "afplay"))
        org-pomodoro-play-sounds t           ; Determines whether soudns are played or not
				org-pomodoro-keep-killed-pomodoro-time t
				org-pomodoro-format " %s"
				org-pomodoro-short-break-format " Short Break %s"
				org-pomodoro-long-break-format  " Long Break %s"
				;; org-pomodoro-finished-sound-p t
        ;; org-pomodoro-start-sound "/home/vberezhnev/.emacs.d/sounds/bell.mp3"

        org-pomodoro-start-sound-p t         ; Determine whether to play a sound when a pomodoro started
        org-pomodoro-start-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)
        org-pomodoro-length 40                ; The length of a pomodoro in minutes

        org-pomodoro-finished-sound-p t      ; Determines whether to play a sound when a pomodoro finished
        org-pomodoro-finished-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

        org-pomodoro-manual-break t          ; Whether the user needs to exit manually from a running pomodoro to enter a break
        org-pomodoro-overtime-sound-p t      ; Determines whether to play a sound when a pomodoro starts to run overtime
        org-pomodoro-overtime-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

				org-pomodoro-length 40
				org-pomodoro-short-break-length 5
				org-pomodoro-long-break-length 15
				org-pomodoro-long-break-frequency 3
				;;org-pomodoro-expiry-time 30
        ;;org-pomodoro-clock-break t           ; Whether to clock time during breaks
				))

(defun my/org-clock-get-clock-string ()
  (concat " " org-clock-heading))

;; (set-face-attribute 'org-pomodoro-mode-line nil :foreground my/green)
;; (set-face-attribute 'org-pomodoro-mode-line-overtime nil :foreground my/red)

;; ────────────────────────────── Prettify Symbols ─────────────────────────────
;; Beautify Org Checkbox Symbol
(defun ma/org-buffer-setup ()
  "Something for like document, i guess 😕."
  (push '("[ ]" . "☐" ) prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(add-hook 'org-mode-hook 'ma/org-buffer-setup)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(evil-leader/set-key
	"nl" 'org-roam-buffer-toggle
	"nf" 'org-roam-node-find
	"ni" 'org-roam-node-insert
	"nc" 'org-roam-capture
	"nt" 'org-roam-tag-add
	"nr" 'org-roam-ref-add
	"nj" 'org-roam-dailies-capture-today
	"ng" 'org-id-get-create
  "nb" 'orb-insert-link)

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c g" . org-id-get-create)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n b" . orb-insert-link)
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

     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
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
      '(("d" "Дневник продуктивности - утро" plain (file "~/Org/Templates/journal/Morning.org")
        :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

        ("D" "Дневник продуктивности - вечер" plain (file "~/Org/Templates/journal/Evening.org")
        :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

        ("j" "Мысли" plain "* %U"
         :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))))
  :config
  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
        '("title" "citekey"  "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  ;; (setq org-roam-database-connector 'sqlite)
  (org-roam-db-autosync-mode)
                                        ; Show +FILETAG in node list
                                        ; https://github.com/org-roam/org-roam/commit/6f5d65abd9e707b3fdb10092a9fef3b739e143dd
  (setq fill-prefix "")  ;; see https://emacs.stackexchange.com/a/38943/12999
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:60}" 'face 'org-tag)))

  ;;for org-roam-buffer-toggle
  ;;Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  )

(use-package org-download
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-x p m"    . org-download-clipboard)
              ("C-x p o"    . org-download-image))
  :config
  (setq-default org-download-image-dir "./assets-org/"))

(use-package org-cliplink
  :ensure t
  :demand t
  :config
  (setq org-cliplink-max-length 800)
  (global-set-key (kbd "C-x p i") 'org-cliplink))

(evil-leader/set-key
	;; "dc" 'org-gtd-capture
  "dc" (lambda () (interactive) (org-gtd-capture nil "i"))
	"de" 'org-gtd-engage
	"dp" 'org-gtd-process-inbox
	"dn" 'org-gtd-show-all-next
	"ds" 'org-gtd-review-stuck-projects)

(use-package org-gtd
  :ensure t
  :straight (org-gtd :type git
                     :host github
                     :repo "trevoke/org-gtd.el")
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  ;; (org-edna-use-inheritance t)
  (org-gtd-update-ack "3.0.0")
	(org-gtd-areas-of-focus '("PERSONAL" "MERITRANK" "CODING" "EGE"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
	(org-gtd-clarify-show-horizons t)
	(org-gtd-horizons-file "horizons.org")
  :config
  (org-edna-mode)
  :bind (("C-c d c" . (lambda () (interactive) (org-gtd-capture nil "i")))
				 ("C-c d e" . org-gtd-engage)
				 ("C-c d r" . org-gtd-engage-grouped-by-context)
				 ("C-c d p" . org-gtd-process-inbox)
				 :map org-gtd-clarify-map
				 ("C-c c" . org-gtd-organize)))

(use-package org-clock-budget
    :quelpa (org-clock-budget
        :fetcher github
        :repo "Fuco1/org-clock-budget"
        :branch "master")
		:ensure t
    :config
    (setq org-clock-budget-daily-budgetable-hours 10)
    (setq org-clock-budget-intervals '(("BUDGET_WEEK" org-clock-budget-interval-this-week))))

(defun org-dblock-write:time-requirements (params)
  "Generate a table showing daily time requirements and progress for categories."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         (categories '("EGE" "MERITRANK" "CODING"))
         (today-start (format-time-string "%Y-%m-%d"))
         (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))
    
    ;; Создаем заголовок таблицы с фиксированной шириной столбцов
    (insert "| Category   | Required | Actual  | Progress  |\n")
    (insert "|------------+----------+---------+-----------|\n")
    
    (dolist (category categories)
      (let ((required 0.0)
            (actual 0.0))
        ;; Находим требуемое время
        (org-map-entries
         (lambda ()
           (let* ((cat (org-entry-get (point) "CATEGORY"))
                  (req (org-entry-get (point) required-property)))
             (when (and req (string= cat category))
               (setq required (string-to-number req)))))
         nil 'file)
        
        ;; Вычисляем фактическое время
        (setq actual (/ (float (org-clock-sum today-start today-end
                                             (lambda () 
                                               (string= (org-entry-get nil "CATEGORY") 
                                                      category))))
                       60.0))
        
        ;; Вычисляем прогресс
        (let ((progress (if (> required 0.0)
                          (* 100.0 (/ actual required))
                        0.0)))
          ;; Используем фиксированную ширину для каждого столбца
          (insert (format "| %-10s | %8.1f | %7.1f | %8.1f%% |\n"
                         category required actual progress)))))
    
    ;; Добавляем нижний разделитель
    (insert "|------------+----------+---------+-----------|")))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX"))

(use-package rustic
  :ensure t
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
  ;;(setq rustic-format-on-save t)
  ;;(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t))
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))

  (use-package rust-playground
    :ensure t))

(use-package go-mode
  :straight t
	:ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 2)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :after (company go-mode)
	:ensure t
  :straight t)

(use-package go-errcheck
  :after go-mode
	:ensure t
  :straight t)

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
  (global-set-key (kbd "C-c C-f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

(setq display-line-numbers 'relative)
