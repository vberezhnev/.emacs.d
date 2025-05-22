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
(electric-pair-mode)
(global-auto-revert-mode t)

(define-coding-system-alias 'UTF-8 'utf-8)
(setq-default shell-file-name "/bin/fish")

(setq flycheck-highlighting-mode 'lines)
(xterm-mouse-mode t)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(defun frostyx/set-default-font (size)
  (set-face-attribute
   'default nil
   :family "Iosevka"
   ;;:foundry "ADBO"
   :height 150 ;;size
   ;; :weight 'regular ;;'normal
   ;; :width 'normal
   :slant 'normal))

(frostyx/set-default-font 120)

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

(use-package helm :ensure t :config (require 'helm-autoloads))

;;(change-theme 'tsdh-light 'doom-xcode)
;; (change-theme 'doom-one-light 'doom-one)
;; (load-theme 'doom-one-light)


(use-package org
  :straight (:type built-in)
  :bind (("C-c C-x C-j" . org-clock-goto))
  :ensure nil
  :bind (("C-c l"               . org-store-link)
         ("C-c f"               . org-footnote-new)
				 ("C-c u"               . calendar))
	:config
	(setq
	 org-ellipsis " ⤵" ;; ⤵, ᗐ, ↴, ▼, ▶, ⤵, ▾
	 ;; org-roam-v2-ack t                 ; anonying startup message
	 org-log-done 'time                ; I need to know when a task is done
	 org-hide-leading-stars t
	 org-log-into-drawer t
	 org-startup-folded t
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
		(add-to-list 'org-modules 'org-temp t)))

(defun org-dblock-write:time-requirements (params)
  "Generate a table showing daily time requirements and progress for categories."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         (categories '("EGE" "CORE" "ASCENT"))
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

(setq	org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
												 "~/Org/agenda/GTD/gtd_archive_2025"
												 "~/Org/agenda/GTD/gtd_archive_2024"
												 "~/Org/agenda/GTD/org-gtd-tasks.org_archive"))

(use-package org-agenda
  :ensure nil
  :straight (:type built-in)
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :config
  (setq org-agenda-start-on-weekday 1
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
        (quote (:maxlevel 5 :compact t :wstart 0 :link t :formula % :tags nil :properties ("CATEGORY" "EFFORT" "File") :narrow 80 :fileskip0 t))
        org-agenda-scheduled-leaders '("[S]:" "[S] x%3dd.:")
        org-agenda-deadline-leaders '("[D]:" "[D] +%3dd.:" "[D] -%3dd.:")
       	org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date)))

				;; Hide duplicates of the same todo item
				;; If it has more than one of timestamp, scheduled,
				;; or deadline information
				org-agenda-skip-timestamp-if-done t
   			org-agenda-skip-deadline-if-done t
   			org-agenda-skip-scheduled-if-done t
   			org-agenda-skip-scheduled-if-deadline-is-shown t
   			org-agenda-skip-timestamp-if-deadline-is-shown t

       	org-default-notes-file "~/Org/agenda/Notes.org"
       	org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")) ;; "~/Org/agenda/Calendar.org"

	;; Refresh org-agenda after rescheduling a task.
	(defun org-agenda-refresh ()
		"Refresh all `org-agenda' buffers."
		(dolist (buffer (buffer-list))
			(with-current-buffer buffer
				(when (derived-mode-p 'org-agenda-mode)
					(org-agenda-maybe-redo)))))

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.5)
    (set-face-attribute 'org-agenda-date-today nil :height 1.5 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.5))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

  (setq org-agenda-custom-commands
        '(("c" "Getting Things Done (GTD)"
           ((agenda "" ((org-agenda-span 'day)
 												;; (org-agenda-skip-scheduled-if-done nil)
 												;; (org-agenda-skip-deadline-if-done nil)
 												(org-agenda-clockreport-mode t)
 												(org-agenda-remove-tags t)
 												(org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
 												(org-time-budgets-in-agenda-maybe)
 												(org-agenda-include-deadlines t)
 												;; (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
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
 									((org-agenda-overriding-header "\nCompleted today\n")))

						(gtd-add-progress-info-to-agenda "")))

          ("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        ;; (org-agenda-finalize-hook '(hq-add-quest-info-to-agenda hq-update-quest-info))
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
       										 (:discard (:not (:tag "habit")))))))
            (hq-add-quest-info-to-agenda "")))

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

   				;; ("k" "Time Tracking Overview"
          ;;  my/time-tracking-view)

          ("d" "Day results"
           ((agenda ""
                    ((org-agenda-span 'day)
 										 (org-agenda-overriding-header "\n === TIME REPORT ===")
 										 (org-agenda-skip-scheduled-if-done nil)
 										 (org-log-done 'time)
 										 (org-log-into-drawer nil)
 										 (org-agenda-skip-deadline-if-done nil)
 										 ;;(org-agenda-block-separator nil)
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
 															 :link t
 															 :formula %
 															 :tags nil
															 :hidefiles t
 															 :properties ("CATEGORY" "EFFORT")))
 										 (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
 																				 "~/Org/agenda/GTD/gtd_archive_2025"
 																				 "~/Org/agenda/GTD/gtd_archive_2024"
 																				 "~/Org/agenda/GTD/org-gtd-tasks.org_archive"))
 										 (org-super-agenda-groups '((:discard (:anything))))))
 						(my/time-tracking-view "")
 						(tags "CLOSED>=\"<today>\""
 									((org-agenda-overriding-header "\n === COMPLETED TASKS ===")
 									 (org-agenda-remove-tags t)))))))

  (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

(use-package timeblock
  :init
  (unless (package-installed-p 'timeblock)
    (package-vc-install
     '(timeblock
       :vc-backend Git
       :url "https://github.com/ichernyshovvv/timeblock.el"
       :branch "master"))))

(use-package org-timeblock
  :straight (org-timeblock :type git
 													 :host github
 													 :repo "ichernyshovvv/org-timeblock")
  :demand t
  :bind
  (:map global-map
 				("C-c s" . org-timeblock))
  :config
  (setq org-now-location '("~/Org/agenda/timeblock.org")
        org-timeblock-inbox-file "~/Org/agenda/Calendar.org"
        org-timeblock-n-days-view 7))

(org-timeblock)
