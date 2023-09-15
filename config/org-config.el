;;________________________________________________________________
;;    Setup org-mode
;;________________________________________________________________
(use-package org
  :bind
  (:map global-map
        ("C-c l" . org-store-link)
        ("C-c c" . org-capture)
        ("M-q" . toggle-truncate-lines)
        ;; Timer (Pomodoro)
        ("C-c t s" . org-timer-set-timer)
        ("C-c t SPC" . org-timer-pause-or-continue)
        ("C-c t <deletechar>") org-timer-stop)
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
     org-modern-todo nil
     org-modern-tag t
     org-modern-timestamp t
     org-modern-statistics t
     org-modern-table nil
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

  (with-eval-after-load 'org
    (setq org-log-done 'time))
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
        '((sequence "TODO(t)" "DOING(d)" "NEXT(n)" "WAITING(w)" "STOPPED(s)" "REVIEW(r)" "|" "DONE" "ARCHIVED(a)" "CANCELLED(c)")))

  (use-package org-fancy-priorities
    :diminish
    :demand t
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))

  (use-package ob-typescript)
  (use-package ob-rust)
  (use-package ob-sql-mode)
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

  (setq org-clock-sound "~/.emacs.d/sounds/sound.wav")
  (use-package org-alert)
  (use-package org-wild-notifier
    :demand t)

  (use-package org-transclusion
    :after org
    :config
    (define-key global-map (kbd "<f12>") #'org-transclusion-add)
    (define-key global-map (kbd "C-n t") #'org-transclusion-mode))

  (use-package org-download
    :demand t
    :config
    (setq-default org-download-image-dir "./assets-org/"))

  (use-package focus
    :demand t
    :config
    '((prog-mode . defun) (text-mode . sentence)))
  (use-package org-cliplink
    :demand t)
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
    :ensure t)
  (use-package org-bullets
    ;; :custom
    ;; (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
    :hook (org-mode . org-bullets-mode)))

(use-package org-agenda
  :ensure nil
  :demand t
  :defer t
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config
  (use-package org-super-agenda
    :demand t)

  (setq org-agenda-files '("~/Org/agenda"))
  (setq org-directory "~/Org")
  (setq org-cycle-separator-lines 2)
  ;; (setq org-agenda-include-diary t) ;; Calendar/Diary integration
  ;; (setq org-default-notes-file "~/Org/agenda/notes.org")

  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode nil)
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
        org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t% s") ;; use "%i %-12:c%?-12t%b% s" to display path
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                 "\n"
                                                 (org-agenda-format-date-aligned date))))
  (setq org-agenda-custom-commands
        '(
          ("z" "Hugo view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  ;; :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(;; Each group has an implicit boolean OR operator between its selectors.
                            ;; (:name "Work important"
                            ;;        :and (:priority>= "B" :category "work" :todo ("TODO" "NEXT")))
                            ;; (:name "Work other"
                            ;;        :and (:category "work" :todo ("TODO" "NEXT")))
                            (:name "Work"
                                   :and (:category "work"))
                            (:name "Important"
                                   :priority "A")
                            (:priority<= "B"
                                         ;; Show this section after "Today" and "Important", because
                                         ;; their order is unspecified, defaulting to 0. Sections
                                         ;; are displayed lowest-number-first.
                                         :order 1)
                            (:name "Deadline Future"
                                   :deadline future)
                            (:name "Today deadline"
                                   :deadline today
                                   :face (:background "black"))
                            (:name "Passed deadline"
                                   :and (:deadline past)
                                   :face (:background "firebrick"))
                            (:name "Stopped tasks"
                                   :and (:todo "STOPPED"))
                            ;; (:name "Papers"
                            ;;        :file-path "~/Org/Org-roam")
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 9)
                            (:name "On review"
                                   :todo "REVIEW"
                                   :order 10)))))))
          ("x" "Zetttel management"
           (  (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '(
                              (:name "TODO"
                                     :file-path "~/Org/Org-roam")
                              (:name "Books"
                                     :category "Book"
                                     :file-path "~/Org/Org-roam")
                              (:name "On review"
                                     :todo "REVIEW"
                                     :order 10
                                     :file-path "~/Org/Org-roam")))))))))
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(setq org-capture-templates
      '(("d" "Daily task" entry (file+function
                                 "~/Org/agenda/PlannedDay.org"
                                 (lambda ()
                                   (org-datetree-find-date-create
                                    (org-date-to-gregorian (org-today)) t)
                                   (re-search-forward "^\\*.+ log" nil t)))
         "* TODO something\nSCHEDULED: <%<%Y-%m-%d>>")))
