;;; org-agenda.el --- Org-agenda configuration -*- lexical-binding: t; -*-

(use-package org-ql
  :straight t)

;; Org-super-agenda: Load for agenda enhancements
(use-package org-super-agenda
  :straight t
  ;; :commans (org-super-agenda-mode)
  :hook (org-agenda-mode . org-super-agenda-mode))

;; (use-package better-org-habit
;;   :straight (:type built-in)
;;   :ensure nil
;;   ;; :straight (better-org-habit :type git :host github :repo "vberezhnev/better-org-habit.el")
;;   :load-path "~/Templates2/Lisp/better-org-habit/better-org-habit.el")

(with-eval-after-load 'org-agenda
  (load-file "~/Templates2/Lisp/better-org-habbit/better-org-habit-custom.el")
  (load-file "~/Templates2/Lisp/better-org-habbit/better-org-habit-stats.el")
  (load-file "~/Templates2/Lisp/better-org-habbit/better-org-habit.el")

  (setq hq-categories
        '((:name "CHINESE" :color "#FF4500" :xp 60 :gold 30)  ; Самые высокие награды
          (:name "ASCENT" :color "#4CAF50" :xp 40 :gold 15)   ; Высокие награды
          (:name "CORE" :color "#FFD700" :xp 35 :gold 10)     ; Средние награды
          (:name "PERSONAL" :color "#6A5ACD" :xp 20 :gold 8))) ; Низкие награды

  ;; Настройка категорий магазина
  (setq hq-market-categories
        '((:id "rest" :name "Rest" :icon "🌟" :description "Способы отдохнуть и восстановиться")
          (:id "entertainment" :name "Entertainment" :icon "🎮" :description "Развлечения")
          (:id "learning" :name "Learning" :icon "📚" :description "Ресурсы для саморазвития и китайского")
          (:id "bonus" :name "Bonuses" :icon "✨" :description "Особые привилегии")
          (:id "rare" :name "Rare Items" :icon "💎" :description "Уникальные предметы")))

  ;; Настройка предметов магазина
  (setq hq-market-items
        '((:id "break-30" :name "30-минутный перерыв" :cost 30 :category "rest"
               :description "Короткий перерыв для отдыха" :use-message "Расслабьтесь и восстановитесь!")
          (:id "break-60" :name "Часовой отдых" :cost 50 :category "rest"
               :description "Полный час отдыха" :use-message "Наслаждайтесь отдыхом!")
          (:id "nap" :name "Короткий сон" :cost 80 :category "rest"
               :description "15-20 минут сна для восстановления" :use-message "Освежающий сон!")
          (:id "episode" :name "Эпизод сериала" :cost 60 :category "entertainment"
               :description "Посмотрите эпизод любимого сериала" :use-message "Приятного просмотра!")
          (:id "movie" :name "Фильм" :cost 100 :category "entertainment"
               :description "Посмотрите полнометражный фильм" :use-message "Наслаждайтесь фильмом!")
          (:id "chinese-lesson" :name "Урок китайского" :cost 80 :category "learning"
               :description "Дополнительный урок塞尔: 30-минутный урок китайского" :use-message "Учитесь с удовольствием!")
          (:id "self-study" :name "Час саморазвития" :cost 70 :category "learning"
               :description "Час на чтение или курсы" :use-message "Развивайтесь дальше!")
          (:id "delay-1h" :name "Отложить задачу на час" :cost 70 :category "bonus"
               :description "Отложить задачу на 1 час" :use-message "Задача отложена на час!")
          (:id "music" :name "Музыка во время работы" :cost 30 :category "bonus"
               :description "Час музыки во время работы" :use-message "Наслаждайтесь музыкой!")
          (:id "late-wake" :name "Поздний подъём" :cost 150 :category "bonus"
               :description "Разрешение встать на час позже" :use-message "Поспите подольше!")
          (:id "day-off" :name "Выходной день" :cost 500 :category "rare"
               :description "Полный выходной от всех задач" :discountable nil
               :use-message "Наслаждайтесь заслуженным выходным!")
          (:id "weekend" :name "Игровой уикенд" :cost 1000 :category "rare"
               :description "Уикенд для игр" :discountable nil
               :use-message "Весёлого игрового уикенда!")))

  ;; Настройка квестов
  (setq hq-quests
        '((:id 1 :name "Путь к осознанности"
               :description "Выполняйте все три медитации 5 дней подряд"
               :habits ("🌿 - Вечерняя медитация" "🧘 - Утренняя медитация" "🌙 - Дневная медитация")
               :required 5 :progress 0 :completed nil
               :reward-xp 200 :reward-gold 100)
          (:id 2 :name "Ранний подъём"
               :description "Просыпайтесь в 07:00 7 дней подряд"
               :habits ("⏰ - Проснуться в 07:00")
               :required 7 :progress 0 :completed nil
               :reward-xp 250 :reward-gold 120)
          (:id 3 :name "Здоровый баланс"
               :description "Пейте 2 литра воды и делайте 10к шагов 10 дней подряд"
               :habits ("💧 - 2 литра воды" "🚶 - 10к шагов")
               :required 10 :progress 0 :completed nil
               :reward-xp 350 :reward-gold 150)
          (:id 4 :name "Мастер китайского"
               :description "Изучайте китайский 7 дней подряд"
               :habits ("📚 - Изучение китайских слов" "✍️ - Практика китайской грамматики" "🎧 - Аудирование на китайском")
               :required 7 :progress 0 :completed nil
               :reward-xp 500 :reward-gold 300)
          (:id 5 :name "Саморазвитие"
               :description "Работайте над саморазвитием 5 дней подряд"
               :habits ("📖 - Чтение проф. литературы" "🎓 - Онлайн-курсы")
               :required 5 :progress 0 :completed nil
               :reward-xp 400 :reward-gold 200)
          (:id 6 :name "Режим сна"
               :description "Соблюдайте режим сна и принимайте БАДы 7 дней подряд"
               :habits ("😴 - Соблюдение режима сна" "💊 - БАДы")
               :required 7 :progress 0 :completed nil
               :reward-xp 300 :reward-gold 150)))
  (hq-setup))

(defun my/org-super-agenda-date-in-n-days (days)
  "Return a date string for DAYS days from today in YYYY-MM-DD format."
  (format-time-string "%Y-%m-%d" (time-add (current-time) (* days 24 60 60))))

;; Org-agenda: Load for agenda commands
(use-package org-agenda
  :straight (:type built-in)
  ;; :commands (org-agenda)
  :bind (:map global-map
              ("C-c a" . org-agenda))
  :init
  (setq org-agenda-start-on-weekday 1
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator #x2501
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode nil
        org-agenda-deadline-faces
        '((1.0001 . org-warning)
          (0.0 . org-upcoming-deadline))
        org-icalendar-combined-name "Hugo Org"
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)
        org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
        org-icalendar-timezone "Asia/Vladivostok"
        org-icalendar-store-UID t
        org-icalendar-alarm-time 30
        calendar-date-style 'european
        calendar-week-start-day 1
        calendar-mark-holidays-flag t
        calendar-mark-diary-entries-flag nil
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
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-default-notes-file "~/Org/agenda/Notes.org"
        org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org")
	)
  :config

  ;; Add hook to move cursor to top of agenda buffer
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (run-at-time "0.001 sec" nil
                           (lambda ()
                             (when (eq major-mode 'org-agenda-mode)
                               (goto-char (point-min)))))))

  (setq org-agenda-custom-commands
	'(
	  ;; ("c" "📅 Tartarus"
          ;;  ((agenda "" ((org-agenda-span 'day)
	  ;; 		(org-agenda-clockreport-mode t)
	  ;; 		(org-agenda-remove-tags t)
	  ;; 		(org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
	  ;; 		(org-agenda-include-deadlines t)
	  ;; 		(org-super-agenda-groups
	  ;; 		 '((:name "🕒 Schedule"
          ;;                         :time-grid t
          ;;                         :face (:background "#A3C7E8" :foreground "white" :weight bold))  ; Pale sky blue
          ;;                  (:name "📌 Today"
          ;;                         :scheduled today
          ;;                         :face (:background "#B7E1D2" :foreground "white" :weight bold))  ; Very pale seafoam green
          ;;                  (:name "⏳ Future deadline"
          ;;                         :deadline future
          ;;                         :face (:background "#C9DDF7" :foreground "black"))  ; Almost white sky blue
          ;;                  (:name "🚨 Deadline today"
          ;;                         :deadline today
          ;;                         :face (:background "#6E8B8B" :foreground "white"))  ; Pale slate teal
          ;;                  (:name "❌ Passed deadline"
          ;;                         :deadline past
          ;;                         :scheduled past
          ;;                         :face (:background "#F8C6C2" :foreground "white")))))  ; Very pale pink-coral
	  ;; 	    (tags "CLOSED>=\"<today>\""
	  ;; 		  ((org-agenda-overriding-header "\n✅ Completed today\n")
	  ;; 		   (org-agenda-remove-tags t)))
	  ;; 	    (gtd-add-progress-info-to-agenda ""))))
	  
	  ;; Eisenhower Matrix — сортировка задач по важности и срочности:
	  ;; Q1 (🔥🚨 Urgent & Important): задачи с высокой приоритетностью (B или выше) и ближайшими дедлайнами или запланированные на ближайшее время.
	  ;; Q2 (🌟📋 Not Urgent & Important): важные, но не срочные задачи, без ближайших дедлайнов и не запланированные.
	  ;; Q3 (⏰⚠️ Urgent & Not Important): срочные, но неважные задачи, с приближающимся дедлайном или запланированные на ближайшее время.
	  ;; Q4 (🌿📝 Not Urgent & Not Important): неважные и несрочные задачи, без ближайших дедлайнов и не запланированные.
	  ;; Задачи со статусом DONE или CANCELLED отбрасываются.
	  ;;
	  ;; Кастомная функция (my/org-super-agenda-date-in-n-days 3):
	  ;; возвращает дату через N дней от текущей (здесь N=3), чтобы автоматически выделять
	  ;; задачи с дедлайнами или scheduled в ближайшие 3 дня.
	  ("d" "🔲 Eisenhower Matrix"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-remove-tags t)
                     (org-agenda-sorting-strategy '(priority-down deadline-up category-keep))
		     (org-agenda-prefix-format "  ∘ %t %s")
                     (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                     (org-super-agenda-groups
                      `((:name "🔥🚨 Q1: Urgent & Important"
                               :order 1
                               :face (:background "#FF4500" :foreground "white" :weight bold)
                               :and (:priority>= "B" :deadline (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:category ("CHINESE" "CORE") :deadline (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:priority>= "B" :scheduled (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:category ("CHINESE" "CORE") :scheduled (before ,(my/org-super-agenda-date-in-n-days 3))))
			(:name "🌟📋 Q2: Not Urgent & Important"
                               :order 2
                               :face (:background "#FFD700" :foreground "black" :weight bold)
                               :and (:priority>= "B" :not (:deadline t) :not (:scheduled t))
                               :and (:category ("CHINESE" "CORE") :not (:deadline t) :not (:scheduled t)))
			(:name "⏰⚠️ Q3: Urgent & Not Important"
                               :order 3
                               :face (:background "#6A5ACD" :foreground "white")
                               :and (:priority<= "C" :deadline (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:category ("ASCENT" "PERSONAL") :deadline (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:priority<= "C" :scheduled (before ,(my/org-super-agenda-date-in-n-days 3)))
                               :and (:category ("ASCENT" "PERSONAL") :scheduled (before ,(my/org-super-agenda-date-in-n-days 3))))
			(:name "🌿📝 Q4: Not Urgent & Not Important"
                               :order 4
                               :face (:background "#4CAF50" :foreground "white")
                               :and (:priority<= "C" :not (:deadline t) :not (:scheduled t))
                               :and (:category ("ASCENT" "PERSONAL") :not (:deadline t) :not (:scheduled t)))
			(:discard (:todo ("DONE" "CANCELLED")))))))
            (alltodo ""
                     ((org-agenda-overriding-header "📋 Tasks without dates")
                      (org-super-agenda-groups
                       `((:name "🌟📋 Q2: Not Urgent & Important"
				:order 2
				:face (:background "#FFD700" :foreground "black" :weight bold)
				:and (:priority>= "B" :not (:deadline t) :not (:scheduled t))
				:and (:category ("CHINESE" "CORE") :not (:deadline t) :not (:scheduled t)))
			 (:name "🌿📝 Q4: Not Urgent & Not Important"
				:order 4
				:face (:background "#4CAF50" :foreground "white")
				:and (:priority<= "C" :not (:deadline t) :not (:scheduled t))
				:and (:category ("ASCENT" "PERSONAL") :not (:deadline t) :not (:scheduled t)))
			 (:discard (:todo ("DONE" "CANCELLED")))))))))

          ("x" "🧘 Habits view"
           ((agenda "" ((org-agenda-span 'day)
			(org-habit-show-habits t)
			(org-agenda-remove-tags t)
			(org-agenda-prefix-format "  ∘ %t %s")
			(org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
			(org-super-agenda-groups
			 '((:name "🌍 Everytime"
                                  :tag ("everytime"))
                           ;; :face (:background "#A8BFC7" :foreground "white" :weight bold))  ; Pale blue-gray
                           (:name "🌅 Morning"
                                  :tag ("morning"))
                           ;; :face (:background "#C7E8D5" :foreground "black"))  ; Very pale green
                           (:name "☀️ Day"
                                  :tag ("day"))
                           ;; :face (:background "#F0E9C9" :foreground "black"))  ; Pale ivory
                           (:name "🌙 Evening"
                                  :tag ("evening"))
                           ;; :face (:background "#D1E4F5" :foreground "black"))  ; Very pale blue
                           (:discard (:anything))
                           (:discard (:not (:tag "habit"))))))
		    (hq-add-quest-info-to-agenda ""))))

          ;; ("d" "📊 Day results"
          ;;  ((agenda ""
          ;;           ((org-agenda-span 'day)
          ;;            (org-agenda-overriding-header "\n📈 === TIME REPORT ===\n")
          ;;            (org-agenda-skip-scheduled-if-done nil)
          ;;            (org-log-done 'time)
          ;;            (org-log-into-drawer nil)
          ;;            (org-agenda-skip-deadline-if-done nil)
          ;;            (org-agenda-clockreport-mode t)
          ;;            (org-agenda-remove-tags t)
          ;;            (org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))
          ;;            (org-agenda-include-deadlines t)
          ;;            (org-agenda-clockreport-parameter-plist
          ;;             '(:scope ("~/Org/agenda/GTD/org-gtd-tasks.org"
	  ;; 			"~/Org/agenda/GTD/gtd_archive_2025"
	  ;; 			"~/Org/agenda/GTD/gtd_archive_2024"
	  ;; 			"~/Org/agenda/GTD/org-gtd-tasks.org_archive"
	  ;; 			"~/Documents/1 People/Арсен Маркарян/Files/Lobby.org")
	  ;; 		       :maxlevel 5
	  ;; 		       :emphasize t
	  ;; 		       :block day
	  ;; 		       :compact t
	  ;; 		       :wstart 0
	  ;; 		       :link t
	  ;; 		       :formula %
	  ;; 		       :tags nil
	  ;; 		       :hidefiles t
	  ;; 		       :properties ("CATEGORY" "EFFORT")))
          ;;            (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"
	  ;; 				 "~/Org/agenda/GTD/gtd_archive_2025"
	  ;; 				 "~/Org/agenda/GTD/gtd_archive_2024"
	  ;; 				 "~/Org/agenda/GTD/org-gtd-tasks.org_archive"))
          ;;            (org-super-agenda-groups '((:discard (:anything))))))
          ;;   (my/time-tracking-view "")
          ;;   (tags "CLOSED>=\"<today>\""
          ;;         ((org-agenda-overriding-header "\n✅📋 === COMPLETED TASKS ===\n")
          ;;          (org-agenda-remove-tags t)))))

          ("E" "📤 View for exporting"
           ((agenda "" ((org-agenda-span 'week)
			(org-agenda-prefix-format
			 '((agenda . "| % t")
                           (todo . "%s")
                           (tags . "%t %-10c | %s")
                           (search . "%c %t %s")))
			(org-agenda-clockreport-parameter-plist
			 '(:maxlevel 5 :compact t :wstart 0 :link t :formula % :tags nil
                                     :properties ("CATEGORY" "EFFORT" "File")
                                     :narrow 80 :fileskip0 t))
			(org-agenda-scheduled-leaders '("[S]:" "[S] x%3dd.:"))
			(org-agenda-deadline-leaders '("[D]:" "[D] +%3dd.:" "[D] -%3dd.:"))
			(org-agenda-clockreport-mode nil)
			(org-agenda-remove-tags t)
			(org-agenda-filter '(category "+CORE"))
			(org-super-agenda-groups
			 '((:name "📅📌 CORE Tasks"
                                  :category "CORE"
                                  :face (:background "#F5E0A9" :foreground "black" :weight bold))))))))
	  
	  ("w" "✅ Weekly Completed Tasks"
	   ((tags "TODO=\"DONE\"&CLOSED>=\"<-1w>\"")))

	  ("m" "📅 Monthly Completed Tasks"
	   ((tags "TODO=\"DONE\"&CLOSED>=\"<-1m>\"")))
	  ))

  (defun my/style-org-agenda()
    (set-face-attribute 'org-agenda-date nil :height 1.5)
    (set-face-attribute 'org-agenda-date-today nil :height 1.5 :slant 'italic)
    (set-face-attribute 'org-agenda-date-weekend nil :height 1.5))
  (add-hook 'org-agenda-mode-hook 'my/style-org-agenda))

(defun my/time-tracking-view (&optional arg)
  "Create a dedicated time tracking view with colorful styling."
  (let* ((day-of-week (upcase (format-time-string "%^a")))
	 (required-property (concat "REQUIRED_TIME_" day-of-week))
	 (categories '("CHINESE" "CORE" "ASCENT"))
	 (today-start (format-time-string "%Y-%m-%d"))
	 (today-end (format-time-string "%Y-%m-%d" (time-add (current-time) 86400))))
    (org-agenda-prepare "Time Tracking")
    (insert
     (propertize "============================\n"
		 'face '(:foreground "#4A90E2")))
    (insert
     (propertize "🕰️ TIME TRACKING DASHBOARD 🕰️\n"
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
		 ((string= category "CHINESE") "#FF4500")
		 ((string= category "CORE") "#CE412B")
		 ((string= category "ASCENT") "#32CD32")
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

(use-package org-hyperscheduler
  :straight
  ( :repo "dmitrym0/org-hyperscheduler"
    :host github
    :type git
    :files ("*"))
  :config
  (setq org-hyperscheduler-inbox-file "~/Org/agenda/timeblock.org")
  (setq org-hyperscheduler-readonly-mode nil))

(provide 'org-agenda)
;;; org-agenda.el ends here
