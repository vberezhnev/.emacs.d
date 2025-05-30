(require 'org-macs)

(require 'cl-lib)
(require 'org)
(require 'org-agenda)

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
	 ;; org-src-fontify-natively t
	 ;; org-src-tab-acts-natively t
	 org-cycle-separator-lines 2
	 org-startup-with-inline-images t
	 org-display-remote-inline-images t
	 ;; org-src-preserve-indentation nil
	 ;; org-edit-src-content-indentation 2
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
		(add-to-list 'org-modules 'org-tempo t)))

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

(defun my/time-tracking-view (&optional arg)
  "Create a dedicated time tracking view with colorful styling."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         (categories '("EGE" "CORE" "ASCENT" "PERSONAL"))
         (main-categories '("EGE" "CORE" "ASCENT"))
         (today-start (format-time-string "%Y-%m-%d"))
         (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))

    ;; Очищаем буфер и устанавливаем заголовок
    (org-agenda-prepare "Time Tracking")

    ;; Красочный разделитель
    (insert
     (propertize "============================\n"
                 'face '(:foreground "#4A90E2")))

    ;; Заголовок с цветом и стилем
    (insert
     (propertize "🕰️ TIME TRACKING DASHBOARD 🕰️\n"
                 'face '(:foreground "#4A90E2"
																		 :weight bold
																		 :height 1.2)))

    ;; Дата с цветовым оформлением
    (insert
     (propertize (format-time-string "📅 Date: %Y-%m-%d\n\n")
                 'face '(:foreground "#2196F3")))

    (let ((total-time 0.0)
          (category-data '())
          (most-active-cat nil)
          (most-active-hours 0.0)
          (total-tasks 0)
          (total-required 0.0))

      ;; Собираем данные по каждой категории
      (dolist (category categories)
        (let ((required 0.0)
              (actual 0.0)
              (tasks 0))

          ;; Находим требуемое время (кроме PERSONAL)
          (unless (string= category "PERSONAL")
            (org-map-entries
             (lambda ()
               (let* ((cat (org-entry-get (point) "CATEGORY"))
                      (req (org-entry-get (point) required-property)))
                 (when (and req (string= cat category))
                   (setq required (+ required (string-to-number req))))))
             nil 'agenda))

          ;; Вычисляем фактическое время и считаем задачи
          (dolist (file (org-agenda-files))
            (with-current-buffer (org-get-agenda-file-buffer file)
              (org-clock-sum today-start today-end
                             (lambda ()
                               (string= (org-entry-get nil "CATEGORY")
                                        category)))
              (setq actual (+ actual (/ (float org-clock-file-total-minutes) 60.0)))

              ;; Подсчёт задач (кроме PERSONAL)
              (unless (string= category "PERSONAL")
                (org-map-entries
                 (lambda ()
                   (when (string= (org-entry-get nil "CATEGORY") category)
                     (setq tasks (1+ tasks))))
                 nil 'file))))

          ;; Обновляем статистику только для основных категорий
          (unless (string= category "PERSONAL")
            (when (> actual most-active-hours)
              (setq most-active-cat category
                    most-active-hours actual))
            (setq total-time (+ total-time actual))
            (setq total-tasks (+ total-tasks tasks))
            (setq total-required (+ total-required required)))

          (push (list category actual required tasks) category-data)))

      ;; Заголовок категорий с цветом
      (insert
       (propertize "📊 Time Breakdown\n"
                   'face '(:foreground "#2196F3"
																			 :weight bold)))

      ;; Основная таблица с цветным оформлением
      (insert "| Category   | Required | Actual  | Progress  |\n")
      (insert "|------------+----------+---------+-----------|\n")

      (dolist (data (nreverse category-data))
        (let* ((category (nth 0 data))
               (actual (nth 1 data))
               (required (nth 2 data)))
          (if (string= category "PERSONAL")
              (insert (format "| %-10s | %8s | %7.1f | %9s |\n"
                              category "---" actual "---"))
            (let ((progress (if (> required 0.0)
                                (* 100.0 (/ actual required))
                              0.0))
                  (cat-color
                   (cond
                    ((string= category "EGE") "#FF6B6B")
                    ((string= category "CORE") "#4CAF50")
                    ((string= category "ASCENT") "#2196F3")
                    (t "#333333"))))
              (insert
               (format "| %s | %8.1f | %7.1f | %8.1f%% |\n"
                       (propertize (format "%-10s" category)
                                   'face `(:foreground ,cat-color
																											 :weight bold))
                       required actual progress))))))

      ;; График бюджета времени
      (insert "\n")
      (insert
       (propertize "📅 Time Budget Graph\n"
                   'face '(:foreground "#2196F3"
																			 :weight bold)))

      (insert (save-window-excursion
                (with-temp-buffer
                  (org-clock-budget-report)
                  (buffer-string))))

      ;; Расширенная статистика
      (insert "\n")
      (insert
       (propertize "📈 Productivity Statistics\n"
                   'face '(:foreground "#2196F3"
																			 :weight bold)))

      ;; Статистика с цветовым оформлением
      (when most-active-cat
        (insert
         (propertize (format "🏆 Most productive category: %s (%.1f hours)\n"
                             most-active-cat most-active-hours)
                     'face '(:foreground "#4CAF50"))))

      (when (> total-tasks 0)
        (insert
         (propertize (format "⏱️ Average time per task: %.1f minutes\n"
                             (/ (* total-time 60) total-tasks))
                     'face '(:foreground "#FF9800"))))

      ;; Общий прогресс с цветовым оформлением
      (let ((total-progress (if (> total-required 0)
                                (* 100.0 (/ total-time total-required))
                              0)))
        (insert
         (propertize (format "📊 Overall progress: %.1f%% (%.1f/%.1f hours)\n"
                             total-progress total-time total-required)
                     'face `(:foreground
                             ,(cond
                               ((>= total-progress 80) "#4CAF50")
                               ((>= total-progress 50) "#FF9800")
                               (t "#FF5722")))))))))

(global-set-key (kbd "C-c C-x o") 'org-clock-out)
(global-set-key (kbd "C-c C-x j") 'org-clock-go-to)

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode 1))

;; (add-to-list 'load-path "~/Templates2/Lisp/org-habit-enhanced")
;; (require 'org-habit-core)
;; (require 'org-habit-enhanced)
;; (require 'org-habit-stats)
;; (require 'org-habit-quest)
;; (require 'org-habit-tasks)
;; (require 'org-habit-market)
;; (hq-setup)

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
  (load "~/.emacs.d/lisp/packages/my-org-habit")
  (setq org-habit-following-days 1
        org-habit-preceding-days 6
        org-habit-show-habits nil
        org-habit-show-all-today t
        org-habit-graph-column 60
        org-habit-overdue-glyph ?○
        org-habit-alert-glyph ?○
        org-habit-ready-future-glyph ?○ ;;⬡
        org-habit-today-glyph ?◎
        org-habit-completed-glyph ?●
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
  :load-path "~/.emacs.d/lisp/packages/"
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

          ;; Добавляем разделитель после календаря
          (insert "\n================================\n\n")
					)))))

;; Регистрируем функцию как хук финализации agenda
(add-hook 'org-agenda-finalize-hook 'my-display-work-habit-calendar)

(load "~/.emacs.d/lisp/gamifications/quest-system-core")
(load "~/.emacs.d/lisp/gamifications/market")
(load "~/.emacs.d/lisp/gamifications/quest-ui")
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

(use-package org-ref
	:quelpa (org-ref
					 :fetcher github
					 :repo "jkitchin/org-ref"
					 :branch "master")
	;; :ensure nil
  :config
	(require 'org-ref)
	;; (require 'org-ref-helm)
	(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

	(setq bibtex-completion-bibliography '("~/Org/Bibliography/Bibliography.bib")
				bibtex-completion-library-path '("~/Org/Bibliography/files/")
				bibtex-completion-notes-path "~/Org/Bibliography/notes/"
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
					(call-process "open" nil 0 nil fpath)))
	(setq bibtex-autokey-year-length 4
				bibtex-autokey-name-year-separator "-"
				bibtex-autokey-year-title-separator "-"
				bibtex-autokey-titleword-separator "-"
				bibtex-autokey-titlewords 2
				bibtex-autokey-titlewords-stretch 1
				bibtex-autokey-titleword-length 5))

(use-package org-roam-bibtex
  :after org-roam
	:quelpa (org-roam-bibtex
					 :fetcher github
					 :repo "org-roam/org-roam-bibtex"
					 :branch "main"))
;;:config
;;(require 'org-ref) ; optional: if using Org-ref v2 or v3 citation links

(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))

(org-link-set-parameters "zotero" :follow 'my-org-zotero-open)

(use-package citar
	:quelpa (citar
					 :fetcher github
					 :repo "emacs-citar/citar"
					 :branch "main")
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Org/Bibliography/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :config
  (defvar citar-indicator-notes-icons
		(citar-indicator-create
		 :symbol (all-the-icons-material
							"speaker_notes"
							:face 'all-the-icons-blue
							:v-adjust -0.3)
		 :function #'citar-has-notes
		 :padding "  "
		 :tag "has:notes")))

;; (use-package tsc
;;   :ensure t)

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
   ;; (solidity   . t)
   ;; (typescript . t)
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
        ("@mathematics" . ?m)
        ("@english" . ?e)
        ("@zettelkasten" . ?z)
  			("@idea" . ?i)))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
	:after org
  :ensure t
	:init
	(global-org-modern-mode 1)
  :config
  (setq org-catch-invisible-edits 'show-and-error
				;; org-special-ctrl-a/e t
				;; Appearance
				org-modern-radio-target    '("❰" t "❱")
				org-modern-internal-target '("↪ " t "")
				org-modern-block-name '((t . t)
  															("src" "ϰ" "ϰ")
																("quote" "❝" "❞"))
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
				org-modern-tag-faces (quote (("@article" :background "#0b8043" :foreground "#000000")
  																	 ("@mathematics" :background "#bc8f8f" :foreground "#000000")
																		 ("blockchain" :background "#f5511d" "#000000")
  																	 ("solana" :background "#DC1FFF" :foreground "#000000")
  																	 ("rust" :background "#CE412B" :foreground "#000000")
  																	 ("go" :background "#00bfff" :foreground "#00000")))
				org-modern-horizontal-rule "──────────────────────────────────────────────────────────────────────────────────────────"
				org-modern-hide-stars " "
				org-modern-keyword "‣"
				org-modern-table t))
;; (global-org-modern-mode t)

;;(frostyx/guix :install "alsa-utils")

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

        org-pomodoro-finished-sound-p t      ; Determines whether to play a sound when a pomodoro finished
        org-pomodoro-finished-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

        org-pomodoro-manual-break t          ; Whether the user needs to exit manually from a running pomodoro to enter a break
        org-pomodoro-overtime-sound-p t      ; Determines whether to play a sound when a pomodoro starts to run overtime
        org-pomodoro-overtime-sound (expand-file-name "sounds/bell.wav" user-emacs-directory)

				org-pomodoro-length 40
				org-pomodoro-short-break-length 5
				org-pomodoro-long-break-length 15
				org-pomodoro-long-break-frequency 3))

(defun my/org-pomodoro ()
  (interactive)
  (org-pomodoro '(4)))

(use-package org-timed-alerts
  :straight (:host github
									 :repo "legalnonsense/org-timed-alerts"
									 :branch "master" :files ("*.el" "out"))
  :after (org)
  :custom
  (org-timed-alerts-alert-function #'alert)
  (org-timed-alerts-tag-exclusions nil)
  (org-timed-alerts-default-alert-props nil)
  (org-timed-alerts-warning-times '(-30 -15 -5))
  (org-timed-alerts-agenda-hook-p t)
  (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time"))
  :config
  (add-hook 'org-mode-hook #'org-timed-alerts-mode))

(use-package emacsql
  :ensure t)
;; (use-package sqlite3
;;   :ensure t)

(use-package org-roam
  :ensure t
	:demand t
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
  (setq fill-prefix "")
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
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-timestamps
  :ensure t
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

;; (use-package org-roam-ui
;;   :straight
;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

;; (use-package org-readwise
;;   :quelpa (org-readwise :fetcher github :repo "CountGreven/org-readwise")
;;   :config
;;   ;; Ensure auth-source is configured to find your Readwise token
;;   (setq auth-sources '("~/.authinfo"))

;; Set the output location for your highlights (buffer or file)
;; (setq org-readwise-output-location "~/Org/readwise-highlights.org")

;; ;; Optionally set the debug level (0 = no debug, 1 = basic debug, 2 = detailed debug)
;; (setq org-readwise-debug-level 1))

(use-package restclient
  :ensure t)

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

(setq org-gtd-update-ack "3.0.0")

(use-package org-gtd
  :ensure t
	:demand t
  :straight (org-gtd :type git
                     :host github
                     :repo "trevoke/org-gtd.el")
  :custom
  (org-gtd-directory "~/Org/agenda/GTD/")
  (org-edna-use-inheritance t)
  (org-gtd-update-ack "3.0.0")
	(org-gtd-areas-of-focus '("PERSONAL" "CORE" "ASCENT" "EGE"))
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
	(org-gtd-clarify-show-horizons t)
	(org-gtd-horizons-file "horizons.org")
  :config
  (org-edna-mode)
  :bind (;;("C-c d c" . (lambda () (interactive) (org-gtd-capture nil "i")))
				 ("C-c d c" . org-gtd-capture)
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

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

;; (use-package habitica
;; 	:ensure t)

(provide 'org-mode)
