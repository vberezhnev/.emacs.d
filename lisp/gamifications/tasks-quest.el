(require 'quest-system-core)

(defvar hq-category-colors
  '(("EGE" . "#FF6B6B")      ; Красный
    ("MERITRANK" . "#4ECDC4") ; Бирюзовый
    ("CODING" . "#45B7D1")    ; Голубой
    ("PERSONAL" . "#96CEB4")) ; Светло-зеленый
  "Цвета для разных категорий.")

(defvar hq-task-rewards
  '(("EGE" . (:xp 50 :gold 40))
    ("MERITRANK" . (:xp 40 :gold 30))
    ("CODING" . (:xp 30 :gold 20))
    ("PERSONAL" . (:xp 20 :gold 10)))
  "Базовые награды за выполнение задач разных категорий.")

(defvar hq-priority-multipliers
  '((?A . 2.0)    ; Приоритет A удваивает награду
    (?B . 1.5)    ; Приоритет B увеличивает награду в 1.5 раза
    (?C . 1.0))   ; Приоритет C оставляет базовую награду
  "Множители наград в зависимости от приоритета задачи.")

(defun hq-calculate-time-bonus (clock-minutes)
  "Рассчитать бонус за потраченное время.
Каждый полный час работы добавляет 10% к награде."
  (let ((hours (/ clock-minutes 60.0)))
    (+ 1.0 (* 0.1 (floor hours)))))

(defun hq-calculate-deadline-bonus ()
  "Рассчитать бонус за соблюдение дедлайна.
Возвращает 1.5 если задача завершена в срок, 1.0 если просрочена или без дедлайна."
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (if (and deadline
             (time-less-p (current-time)
													(org-time-string-to-time deadline)))
        1.5  ; Задача завершена до дедлайна
      1.0))) ; Задача просрочена или без дедлайна

(defun hq-reward-for-task-completion ()
  "Начислить награду за выполнение задачи."
  (when (and (member org-state '("DONE" "done"))  ; Проверяем, что задача завершена
             (not (member "habit" (org-get-tags)))) ; Исключаем привычки
    (let* ((category (or (org-entry-get nil "CATEGORY") "PERSONAL"))
           (base-rewards (cdr (assoc category hq-task-rewards)))
           (priority (org-entry-get nil "PRIORITY"))
           (priority-mult (or (cdr (assoc (and priority (aref priority 0))
																					hq-priority-multipliers))
															1.0))
           ;; Получаем время работы над задачей
           (clock-minutes (org-clock-sum-current-item))
           (time-bonus (hq-calculate-time-bonus clock-minutes))
           (deadline-bonus (hq-calculate-deadline-bonus))

           ;; Рассчитываем финальные награды с учетом всех бонусов
           (final-xp (round (* (plist-get base-rewards :xp)
                               priority-mult
                               time-bonus
                               deadline-bonus)))
           (final-gold (round (* (plist-get base-rewards :gold)
                                 priority-mult
                                 time-bonus
                                 deadline-bonus))))

      ;; Начисляем награды
      (setq hq-xp (+ hq-xp final-xp))
      (setq hq-gold (+ hq-gold final-gold))

      ;; Проверяем повышение уровня
      (let ((new-level (1+ (/ hq-xp 100))))
        (when (> new-level hq-level)
          (setq hq-level new-level)
          (message "🎉 Уровень повышен! Теперь вы %d уровня!" hq-level)))

      ;; Сохраняем данные
      (hq-save-data)

      ;; Выводим информацию о награде
      (message "✨ Задача выполнена! Получено: +%d XP, +%d золота [%s]"
               final-xp final-gold category))))

;; Добавляем хук для отслеживания завершения задач
(add-hook 'org-after-todo-state-change-hook 'hq-reward-for-task-completion)

;; Функция для отображения возможной награды за текущую задачу
(defun hq-show-potential-reward ()
  "Показать потенциальную награду за выполнение текущей задачи."
  (interactive)
		(let* ((inhibit-read-only t))
			;; Добавляем разделитель перед информацией о прогрессе
			(goto-char (point-max))
			(insert "\n"
							(propertize "============================\n"
													'face '(:foreground "#4A90E2")))
			(insert (propertize "🚀 GTD PROGRESS SYSTEM 🚀\n"
													'face '(:foreground "#4A90E2" :weight bold :height 1.2)))

			;; Статистика персонажа с цветовым оформлением
			(insert
			 (propertize (format "👤 Level %d " hq-level)
									 'face '(:foreground "#FFD700" :weight bold))
			 "| "
			 (propertize (format "XP: %d/%d "
													 (mod hq-xp 100)
													 100)
									 'face '(:foreground "#4CAF50" :weight bold))
			 "| "
			 (propertize (format "Gold: %d 🪙\n" hq-gold)
									 'face '(:foreground "#FFD700" :weight bold)))


      (category (or (org-entry-get nil "CATEGORY") "PERSONAL"))
			(base-rewards (cdr (assoc category hq-task-rewards)))
			(priority (org-entry-get nil "PRIORITY"))
			(priority-mult (or (cdr (assoc (and priority (aref priority 0))
																		 hq-priority-multipliers))
												 1.0))
			(clock-minutes (org-clock-sum-current-item))
			(time-bonus (hq-calculate-time-bonus clock-minutes))
			(deadline-bonus (hq-calculate-deadline-bonus))

			(potential-xp (round (* (plist-get base-rewards :xp)
															priority-mult
															time-bonus
															deadline-bonus)))
			(potential-gold (round (* (plist-get base-rewards :gold)
																priority-mult
																time-bonus
																deadline-bonus))))

		(message "💰 Потенциальная награда: %d XP, %d золота [%s]
• Приоритет: x%.1f
• Бонус за время: x%.1f
• Бонус за дедлайн: x%.1f"
						 potential-xp potential-gold category
						 priority-mult time-bonus deadline-bonus))

;; Привязываем функцию просмотра потенциальной награды к удобной комбинации клавиш
(define-key org-mode-map (kbd "C-c r") 'hq-show-potential-reward)

(defun gtd-add-progress-info-to-agenda (&optional arg)
  "Добавить информацию о прогрессе в GTD view с отладочной информацией"
  (interactive)
  (let ((inhibit-read-only t)
        ;; (debug-buffer (get-buffer-create "*GTD Debug*"))
				)
    ;; (with-current-buffer debug-buffer
    ;;   (erase-buffer))
    
    (let* ((categories '("EGE" "MERITRANK" "CODING" "PERSONAL"))
           (total-tasks 0)
           (completed-tasks 0)
           (category-stats (make-hash-table :test 'equal)))

      ;; (defun log-debug (msg &rest args)
      ;;   (with-current-buffer debug-buffer
      ;;     (goto-char (point-max))
      ;;     (insert (apply 'format (concat msg "\n") args))))

      (save-excursion
        (goto-char (point-min))
        ;; (log-debug "Начинаем анализ буфера...")
        
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            
            ;; (log-debug "Проверяем строку: %s" line)
            
            ;; Изменено регулярное выражение для учета начальных пробелов
            (when (string-match "^\\s-*\\([A-Z]+\\)\\s-+|" line)
              (let* ((category (match-string 1 line))
                     (is-completed (or (string-match "DONE" line)
                                     (string-match "done" line)))
                     (stats (or (gethash category category-stats) (list 0 0))))
                
                ;; (log-debug "Найдена задача: Категория=%s, Завершена=%s, line=%s" 
                ;;           category 
                ;;           (if is-completed "да" "нет")
                ;;           line)
                
                (when (member category categories)
                  ;; (log-debug "Обновляем статистику для категории %s" category)
                  (puthash category
                           (list (1+ (car stats))
                                 (if is-completed
                                     (1+ (cadr stats))
                                   (cadr stats)))
                           category-stats)
                  (setq total-tasks (1+ total-tasks))
                  (when is-completed
                    (setq completed-tasks (1+ completed-tasks)))))))
          (forward-line 1)))

      ;; (log-debug "\nИтоговая статистика:")
      ;; (log-debug "Всего задач: %d" total-tasks)
      ;; (log-debug "Выполнено задач: %d" completed-tasks)
      ;; (maphash (lambda (category stats)
      ;;            (log-debug "Категория %s: Всего=%d, Выполнено=%d"
      ;;                      category (car stats) (cadr stats)))
      ;;          category-stats)

      ;; Отображение прогресса
      (goto-char (point-max))
      (insert "\n## Прогресс по категориям\n")
      
      (dolist (category categories)
        (let* ((stats (gethash category category-stats '(0 0)))
               (total (car stats))
               (completed (cadr stats))
               (progress-percent (if (> total 0)
                                   (/ (* completed 100.0) total)
                                 0))
               (quest-bar-width 20)
               (filled-length (round (* quest-bar-width (/ progress-percent 100.0))))
               (empty-length (- quest-bar-width filled-length))
               (category-color (cdr (assoc category hq-category-colors)))
               (base-rewards (cdr (assoc category hq-task-rewards))))
          
          (insert (format "  %d/%d задач " completed total))
          (insert (propertize
                  (concat
                   (make-string filled-length ?▰)
                   (make-string empty-length ?▱))
                  'face `(:foreground ,category-color)))
          (insert (format " %.1f%%\n" progress-percent))
          
          (insert (format "    Награда за задачу: +%d XP, +%d 🪙\n"
                         (plist-get base-rewards :xp)
                         (plist-get base-rewards :gold)))))
      
      ;; Общий прогресс
      (insert "\n## Общий прогресс\n")
      (let* ((overall-progress (if (> total-tasks 0)
                                  (/ (* completed-tasks 100.0) total-tasks)
                                0))
             (quest-bar-width 30)
             (filled-length (round (* quest-bar-width (/ overall-progress 100.0))))
             (empty-length (- quest-bar-width filled-length)))
        
        (insert (format "  %d/%d задач " completed-tasks total-tasks))
        (insert (propertize
                (concat
                 (make-string filled-length ?▰)
                 (make-string empty-length ?▱))
                'face '(:foreground "#4CAF50")))
        (insert (format " %.1f%%\n" overall-progress))))
    
    ;; Показываем отладочный буфер
    ;; (display-buffer debug-buffer)
		))
