(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode 1))

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

(defun my/time-tracking-view (&optional arg)
  "Create a dedicated time tracking view with colorful styling."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
         (required-property (concat "REQUIRED_TIME_" day-of-week))
         ;; Новые категории
         (categories '("CHINESE" "CORE" "ASCENT"))
         (main-categories '("CHINESE" "CORE" "ASCENT"))
         (today-start (format-time-string "%Y-%m-%d"))
         (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))
    
    (org-agenda-prepare "Time Tracking")
    (insert
     (propertize "============================\n"
                 'face '(:foreground "#4A90E2")))
    (insert
     (propertize "🕰️ TIANJIN TIME TRACKING DASHBOARD 🕰️\n"
                 'face '(:foreground "#4A90E2" :weight bold :height 1.2)))
    (insert
     (propertize (format-time-string "📅 Date: %Y-%m-%d\n\n")
                 'face '(:foreground "#2196F3")))
    
    (let ((total-time 0.0)
          (category-data '())
          (most-active-cat nil)
          (most-active-hours 0.0)
          (total-tasks 0)
          (total-required 0.0))
      
      (dolist (category categories)
        (let ((required 0.0)
              (actual 0.0)
              (tasks 0))
          (org-map-entries
           (lambda ()
             (let* ((cat (org-entry-get (point) "CATEGORY"))
                    (req (org-entry-get (point) required-property)))
               (when (and req (string= cat category))
                 (setq required (+ required (string-to-number req))))))
           nil 'agenda)
          (dolist (file (org-agenda-files))
            (with-current-buffer (org-get-agenda-file-buffer file)
              (org-clock-sum today-start today-end
                             (lambda ()
                               (string= (org-entry-get nil "CATEGORY")
                                        category)))
              (setq actual (+ actual (/ (float org-clock-file-total-minutes) 60.0)))
              (org-map-entries
               (lambda ()
                 (when (string= (org-entry-get nil "CATEGORY") category)
                   (setq tasks (1+ tasks))))
               nil 'file)))
          (when (> actual most-active-hours)
            (setq most-active-cat category
                  most-active-hours actual))
          (setq total-time (+ total-time actual))
          (setq total-tasks (+ total-tasks tasks))
          (setq total-required (+ total-required required))
          (push (list category actual required tasks) category-data)))
      
      (insert
       (propertize "📊 Time Breakdown\n"
                   'face '(:foreground "#2196F3" :weight bold)))
      (insert "| Category   | Required | Actual  | Progress  |\n")
      (insert "|------------+----------+---------+-----------|\n")
      
      (dolist (data (nreverse category-data))
        (let* ((category (nth 0 data))
               (actual (nth 1 data))
               (required (nth 2 data))
               (progress (if (> required 0.0)
                             (* 100.0 (/ actual required))
                           0.0))
               (cat-color
                (cond
                 ((string= category "CHINESE") "#FF4500") ; Оранжевый для учебы
                 ((string= category "CORE") "#CE412B")   ; Ржавый для работы
                 ((string= category "ASCENT") "#32CD32") ; Зеленый для отдыха
                 (t "#333333"))))
          (insert
           (format "| %s | %8.1f | %7.1f | %8.1f%% |\n"
                   (propertize (format "%-10s" category)
                               'face `(:foreground ,cat-color :weight bold))
                   required actual progress))))
      
      (insert "\n")
      (insert
       (propertize "📅 Time Budget Graph\n"
                   'face '(:foreground "#2196F3" :weight bold)))
      (insert (save-window-excursion
                (with-temp-buffer
                  (org-clock-budget-report)
                  (buffer-string))))
      
      (insert "\n")
      (insert
       (propertize "📈 Productivity Statistics\n"
                   'face '(:foreground "#2196F3" :weight bold)))
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
          		:box (:line-width (1 . 1) :color "white")))))))
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
          (insert "\n================================\n\n"))))))

;; Регистрируем функцию как хук финализации agenda
(add-hook 'org-agenda-finalize-hook 'my-display-work-habit-calendar)

;; (load "~/.emacs.d/lisp/gamifications/quest-system-core")
;; (load "~/.emacs.d/lisp/gamifications/market")
;; (load "~/.emacs.d/lisp/gamifications/quest-ui")
;; (load "~/.emacs.d/lisp/gamifications/habit-quest")
;; (load "~/.emacs.d/lisp/gamifications/tasks-quest")

;; (use-package habitica
;; 	:ensure t)
