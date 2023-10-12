;;________________________________________________________________
;;    Setup org-mode
;;________________________________________________________________
(use-package org
  :straight (:type built-in)
  :ensure nil
  :defer t
  :after org
  :delight org-mode "✎"
  :demand t
  ;; :pin org
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
	 (org-mode . global-org-modern-mode))
  :bind (("C-c l"               . org-store-link)
	 ("C-c c"               . org-capture)
	 ;; ("С-c a"               . org-agenda)
         ;; ;; Timer (Pomodoro)
         ;; ("C-c t s"             . org-timer-set-timer)
         ;; ("C-c t SPC"           . org-timer-pause-or-continue)
         ;; ("C-c t <deletechar>"  . org-timer-stop)
	 ;; :map global-map
         ;; ("M-q"                 . toggle-truncate-lines)
	 )
  :config
  (setq
   org-ellipsis " ▾" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
   org-roam-v2-ack t                 ; anonying startup message
   ;; org-log-done 'time                ; I need to know when a task is done
   ;; org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-log-into-drawer t
   ;; org-log-done 'time
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
   org-cycle-separator-lines 2
   org-startup-folded 'content
   org-startup-with-inline-images t
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 2
   org-fontify-quote-and-verse-blocks t
   org-export-with-smart-quotes t
   org-image-actual-width '(300))

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

  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
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
  ;;   (setq org-modules
  ;; 	'(org-crypt
  ;;           org-bookmark
  ;;           org-eshell
  ;;           org-irc))

  (use-package org-habit
    :after org
    :ensure nil
    :straight (:type built-in)
    :init
    (add-to-list 'org-modules 'org-habit)
    :config
    (setq org-habit-following-days 7
          org-habit-preceding-days 7
	  org-habit-show-all-today t
          org-habit-show-habits t
	  org-habit-graph-column 67))

  (defun toggle-org-habit-show-all-today ()
    "Toggle the value of `org-habit-show-all-today' between t and nil."
    (interactive)
    (setq org-habit-show-all-today (not org-habit-show-all-today))
    (message "org-habit-show-all-today is now %s"
             (if org-habit-show-all-today "t" "nil"))
    (org-agenda-refresh))

  (define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today)

  (use-package org-habit-stats
    :bind (("Z"               . org-habit-stats-view-next-habit-in-agenda))
    :config
    (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties))

  ;; (use-package  org-habit-plus
  ;;   :straight (:host github :repo "myshevchuk/org-habit-plus" :branch "master")
  ;;   :init
  ;;   (add-to-list 'org-modules 'org-habit-plus))

  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font "Iosevka" :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka" :weight 'medium :height (cdr face)))

  ;; Make sure org-indent face is available
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


  (defvar ol/habit-report-defaultday 30
    "The default range of days from today, when no time is specified.")

  (defun ol/get-org-habit-string (&optional block starttime endtime)
    ;; check if starttime and endtime is specified
    (or starttime (setq starttime (format-time-string "%a %b %e %H:%M:%S %G" (time-subtract (current-time) (days-to-time ol/habit-report-defaultday)))))
    (or endtime (setq endtime (current-time-string)))

    ;; when block is specified set starttime and endtime
    (when block
      (progn
	(setq cc (org-clock-special-range block nil t)
	      starttime (car cc)
	      endtime (nth 1 cc))))

    ;; build the habit graph
    (list (org-habit-build-graph
	   (org-habit-parse-todo)
           ;; time from
	   (org-time-subtract (date-to-time starttime) (* 3600 org-extend-today-until))
           ;; today
	   (date-to-time endtime)
           ;; time to
	   (date-to-time endtime)) starttime endtime))

  (defun ol/habit-report (&optional params)
    (save-excursion
      (org-back-to-heading t)
      (print (ol/get-org-habit-string (plist-get params :block) (plist-get params :tstart) (plist-get params :tend)))
      (let* ((habit-data (ol/get-org-habit-string (plist-get params :block) (plist-get params :tstart) (plist-get params :tend)))
             (habit-str (car habit-data))
             (face-counts (list (cons 'org-habit-clear-future-face  0)
				(cons 'org-habit-ready-face  0)
				(cons 'org-habit-ready-future-face  0)
				(cons 'org-habit-alert-future-face  0)
				(cons 'org-habit-overdue-face  0)))
             (habit-stats (list (cons :org-heading  (org-get-heading t t t t))
				(cons :habit-done  0)
				(cons :habit-missed  0)
				(cons :habit-last-missed  nil)
				(cons :longest-day-streak  0)
				(cons :longest-done-streak  0)
				(cons :current-longest-done-streak  nil)
				(cons :starttime (car (cdr habit-data)))
				(cons :endtime (car (cdr(cdr habit-data))))))
             (cur-day-streak 0)
             (cur-done-streak 0))

	;; iterate over string
	(dotimes (i (length habit-str))
          ;; sum up all faces
          (when (alist-get (get-text-property i 'face habit-str) face-counts)
            (setf (alist-get (get-text-property i 'face habit-str) face-counts) (+ (alist-get (get-text-property i 'face habit-str) face-counts) 1)))
          ;; if face is overdue of alert and has no complete-glyp
          (if (and (or (eq (get-text-property i 'face habit-str)
                           'org-habit-overdue-face)
		       (eq (get-text-property i 'face habit-str)
                           'org-habit-alert-future-face))
                   (not
                    (string= (string (aref habit-str i))
                             (string org-habit-completed-glyph))))
	      (progn
		(setf (alist-get :habit-last-missed habit-stats) (get-text-property i 'help-echo habit-str))
		(when (> cur-day-streak (alist-get :longest-day-streak habit-stats))
                  (setf (alist-get :longest-day-streak habit-stats) cur-day-streak)
                  (setq cur-day-streak 0))
		(when (> cur-done-streak (alist-get :longest-done-streak habit-stats))
                  (setf (alist-get :longest-done-streak habit-stats) cur-done-streak)
                  (setq cur-done-streak 0)))
            (progn
	      (setf cur-day-streak (+ 1 cur-day-streak))
	      (when (eq (get-text-property i 'face habit-str)
			'org-habit-ready-face)
		(setf cur-done-streak (+ 1 cur-done-streak))))
            )
          (if (string= (string (aref habit-str i))
		       (string org-habit-completed-glyph))
	      (setf (alist-get :habit-done habit-stats) (+ 1 (alist-get :habit-done habit-stats))))
          ) ;; string iteration done
	;; when last streak bigger then last streak
	(when (> cur-day-streak (alist-get :longest-day-streak habit-stats))
          (setf (alist-get :longest-day-streak habit-stats) cur-day-streak))
	(when (> cur-done-streak (alist-get :longest-done-streak habit-stats))
          (setf (alist-get :longest-done-streak habit-stats) cur-done-streak)
          (setf (alist-get :current-longest-done-streak habit-stats) t))
	;; set missed habit count
	(setf (alist-get :habit-missed habit-stats) (alist-get 'org-habit-overdue-face face-counts))
	habit-stats)))

  (defun ol/habit-print-header (st et)
    (format "#+CAPTION: Habit report from %s to %s
| Heading | Done Count | Missed Count | Last Missed | Longest Streak (days) | Longest Streak (done) | Currently longest |
|-- |" st et))

  (defun ol/habit-stats-to-string (org-habits)
    (concat (ol/habit-print-header
             (format-time-string "%d-%m-%y" (date-to-time (alist-get :starttime (car org-habits))))
             (format-time-string "%d-%m-%y" (date-to-time (alist-get :endtime (car org-habits)))))
            (let ((result ""))(dolist (org-habit org-habits result)
				(setq result (concat result (format "\n|%s| %S | %s | %s | %s | %s | %s |"
                                                                    (alist-get :org-heading org-habit)
                                                                    (alist-get :habit-done org-habit)
                                                                    (alist-get :habit-missed org-habit)
                                                                    (alist-get :habit-last-missed org-habit)
                                                                    (alist-get :longest-day-streak org-habit)
                                                                    (alist-get :longest-done-streak org-habit)
                                                                    (alist-get :current-longest-done-streak org-habit))))))))

  (defun org-dblock-write:ol/habit-report (params)
    (if (plist-get params :scope)
	(setq ol/scope (plist-get params :scope))
      (setq ol/scope 'tree))
    (insert (ol/habit-stats-to-string
             (org-map-entries (lambda () (ol/habit-report params)) "STYLE=\"habit\"" ol/scope)))
    (org-table-align))

  ;; ────────────────────────────── Prettify Symbols ─────────────────────────────
;;; custom-function
  ;; Beautify Org Checkbox Symbol
  (defun ma/org-buffer-setup ()
    "Something for like document, i guess 😕."
    (push '("[ ]" . "☐" ) prettify-symbols-alist)
    (push '("[X]" . "☑" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist))
  (add-hook 'org-mode-hook #'ma/org-buffer-setup)

;;;; toggle-emphasis
  ;; (defun org-toggle-emphasis ()
  ;;   "Toggle hiding/showing of org emphasis markers."
  ;;   (interactive)
  ;;   (if org-hide-emphasis-markers
  ;; 	(set-variable 'org-hide-emphasis-markers nil)
  ;;     (set-variable 'org-hide-emphasis-markers t))
  ;;   (org-mode-restart))
  ;; (define-key org-mode-map (kbd "C-c x") 'org-toggle-emphasis)

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

  ;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element
  (use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t
          org-appear-autolinks 'just-brackets))

  (use-package org-fancy-priorities
    :diminish
    :demand t
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))

  (use-package org-bullets
    :after org
    ;; :custom
    ;; (org-bullets-bullet-list '("◉" "✿" "✚" "✸" "❀" "○")) ; "●" "▷" "🞛" "◈" "✖"
    :hook (org-mode . org-bullets-mode))

  (use-package toc-org
    :after org
    :init (add-hook 'org-mode-hook 'toc-org-enable))

  ;; (use-package org-transclusion
  ;;   :after org
  ;;   :config
  ;;   (define-key global-map (kbd "<f12>") #'org-transclusion-add)
  ;;   (define-key global-map (kbd "C-n t") #'org-transclusion-mode))

  (use-package org-download
    :demand t
    :config
    (setq-default org-download-image-dir "./assets-org/"))

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

  (setq org-clock-sound "~/.emacs.d/sounds/sound.wav")
  (use-package org-alert)
  (use-package org-wild-notifier
    :demand t)

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

  ;; Log time a task was set to DONE.
  (setq org-log-done (quote time))

  ;; Don't log the time a task was rescheduled or redeadlined.
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)

  (setq org-read-date-prefer-future 'time)

  ;; Refresh org-agenda after rescheduling a task.
  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh))

  ;; (use-package org-rainbow-tags)

  ;; (use-package darkroom)

  ;; (use-package org-books
  ;;   :config
  ;;   (setq org-books-file "~/Org/Reading-list.org"))

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

;; First of all you sould install aplay or afplay
(use-package sound-wav
  :demand t) ;; dep for org-pomodoro
(use-package powershell
  :demand t) ;; dep for org-pomodoro
(require 'sound-wav)
;; (sound-wav-play "/home/chopin.emacs.d/sounds/sound.wav")

(use-package org-pomodoro
  :straight (:host github :repo "marcinkoziej/org-pomodoro"
		   :branch "master")
  :config
  (setq org-pomodoro-length 25)
  (setq org-pomodoro-short-break-length 5)
  (setq org-pomodoro-long-break-length 15)
  (setq org-pomodoro-play-sounds 1)

  (setq org-pomodoro-finished-sound "/home/chopin/.emacs.d/sounds/sound.wav")
  (setq org-pomodoro-long-break-sound "/home/chopin/.emacs.d/sounds/sound.wav")
  (setq org-pomodoro-short-break-sound "/home/chopin/.emacs.d/sounds/sound.wav"))

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
	'("~/Org/agenda/DailyPomodoro.org"
	  "~/Org/agenda/PlanAhead.org"
	  "~/Org/agenda/PlannedDay.org"
  	  "~/Org/agenda/Habits.org"))
  (setq org-cycle-separator-lines 2)
  ;; (setq org-agenda-include-diary t) ;; Calendar/Diary integration
  ;; (setq org-default-notes-file "~/Org/agenda/notes.org")

  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (setq org-agenda-skip-scheduled-if-done nil ;; changed
        org-agenda-skip-deadline-if-done nil ;; changed
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
  calendar-mark-diary-entries-flag t

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
                                  :time-grid t
                                  :date today
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(;; Each group has an implicit boolean OR operator between its selectors.
                            ;; (:name "Work important"
                            ;;        :and (:priority>= "B" :category "work" :todo ("TODO" "NEXT")))
                            ;; (:name "Work other"
                            ;;        :and (:category "work" :todo ("TODO" "NEXT")))
			    ;; (:name "Personal"
                            ;;        :date today
                            ;;        :scheduled today
			    ;; 	   :habit t)
                            ;; (:priority<= "B"
                            ;;              ;; Show this section after "Today" and "Important", because
                            ;;              ;; their order is unspecified, defaulting to 0. Sections
                            ;;              ;; are displayed lowest-number-first.
                            ;;              :order 1)
                            ;; (:name "Papers"
                            ;;        :file-path "~/Org/Org-roam")
			    (:name "Pomodoro"
				   :and (:category "pomodoro" :todo "TODO" :scheduled today :file-path "~/Org/agenda/DailyPomodoro.org"))
                            (:name "Work"
                                   :and (:category "work"))
                            (:name "Important"
                                   :priority "A")
                            (:name "Passed deadline"
                                   :and (:deadline past)
                                   :face (:background "firebrick"))
                            (:name "Today deadline"
                                   :deadline today
                                   :face (:background "black"))
                            (:name "Deadline Future"
                                   :deadline future)
                            (:name "Stopped tasks"
                                   :and (:todo "STOPPED"))
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 9)
                            (:name "On review"
                                   :todo "REVIEW"
                                   :order 10)))))))

          ("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
			 '((:name "Everytime habits"
				  :and (:tag "habits" :tag "everytime"))
			   (:name "Morning habits"
				  :and (:tag "habits" :tag "morning"))
			   (:name "Day habits"
				  :and (:tag "habits" :tag "day"))
			   (:name "Evening habits"
				  :and (:tag "habits" :tag "evening"))))))))))
  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(use-package org-ql)

(defun org-mode-todo-to-done ()
  "Change all TODO keywords to DONE in the current org-mode buffer using org-ql."
  (interactive)
  (org-ql-select "~/Org/agenda/DailyPomodoro.org"
    '(ts :on today)
    :action
    '(lambda () (org-todo "DONE"))))

(run-at-time "23:00" nil 'org-mode-todo-to-done)
(global-set-key (kbd "C-c j") 'org-mode-todo-to-done)

(setq org-capture-templates
      '(("p" "Daily pomodoro" entry (file+function
				     "~/Org/agenda/DailyPomodoro.org"
				     (lambda ()
				       (org-datetree-find-date-create
					(org-date-to-gregorian (org-today)) t)
				       (re-search-forward "^\\*.+ log" nil t)))
	 "* TODO %?\nSCHEDULED: <%<%Y-%m-%d>>")
	("t" "Daily task" entry (file+function
				 "~/Org/agenda/PlannedDay.org"
				 (lambda ()
				   (org-datetree-find-date-create
				    (org-date-to-gregorian (org-today)) t)
				   (re-search-forward "^\\*.+ log" nil t)))
	 "* TODO something\nSCHEDULED: <%<%Y-%m-%d>>")
	("b" "Book" entry (file "~/Org/Reading-list.org")
	 "* %^{TITLE}\n:PROPERTIES:\n:ADDED: <%<%Y-%m-%d>>\n:END:%^{AUTHOR}\n%^{GOODREADS_URL}%?" :empty-lines 1)))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook 'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))


(provide 'org-setting)
