(require 'quest-system-core)

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
  (let* ((category (or (org-entry-get nil "CATEGORY") "PERSONAL"))
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
             priority-mult time-bonus deadline-bonus)))

;; Привязываем функцию просмотра потенциальной награды к удобной комбинации клавиш
(define-key org-mode-map (kbd "C-c r") 'hq-show-potential-reward)

(defun gtd-add-progress-info-to-agenda (&optional arg)
  "Добавить информацию о прогрессе в GTD view"
  (interactive)
  (let ((inhibit-read-only t))
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
                 'face '(:foreground "#FFD700" :weight bold))
     "\n")
    
    ;; Статистика прогресса по категориям
    (let* ((categories '("EGE" "MERITRANK" "CODING" "PERSONAL")) ; Изменен порядок для соответствия приоритетам
           (total-tasks 0)
           (completed-tasks 0)
           (category-stats (make-hash-table :test 'equal))
           (category-colors ; Цвета для каждой категории
            '(("EGE" . "#FF6B6B")      ; Красный
              ("MERITRANK" . "#4ECDC4") ; Бирюзовый
              ("CODING" . "#45B7D1")    ; Голубой
              ("PERSONAL" . "#96CEB4"))) ; Светло-зеленый
           (category-emoji ; Эмодзи для каждой категории
            '(("EGE" . "📚")
              ("MERITRANK" . "🌟")
              ("CODING" . "💻")
              ("PERSONAL" . "🎯"))))
      
      ;; Подсчет задач
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Z]+\\)\\s-+|" nil t)
          (let* ((category (match-string 1))
                 (is-completed (save-match-data 
                                (or (looking-at ".*x [0-9]+d\.: ")
                                    (looking-at ".*DONE"))))
                 (current-count (gethash category category-stats 0)))
            (when (member category categories)
              (puthash category 
                       (list 
                        (1+ (car (gethash category category-stats '(0 0))))
                        (+ (cadr (gethash category category-stats '(0 0)))
                           (if is-completed 1 0)))
                       category-stats)
              (setq total-tasks (1+ total-tasks))
              (when is-completed 
                (setq completed-tasks (1+ completed-tasks)))))))
      
      ;; Отображение прогресса по категориям
      (insert 
       (propertize "## Прогресс по категориям\n" 
                   'face '(:foreground "#2196F3" :weight bold)))
      
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
               (category-color (cdr (assoc category category-colors)))
               (category-icon (cdr (assoc category category-emoji)))
               (base-rewards (cdr (assoc category hq-task-rewards))))
          
          ;; Заголовок категории с эмодзи
          (insert 
           (propertize (format "%s %s\n" category-icon category)
                       'face `(:foreground ,category-color :weight bold)))
          
          ;; Прогресс-бар и статистика
          (insert "  ")
          (insert 
           (propertize (format "%d/%d задач " completed total)
                       'face '(:foreground "#333333")))
          
          (insert 
           (propertize 
            (concat 
             (make-string filled-length ?▰)    ; Жирная полоска для заполненной части
             (make-string empty-length ?▱))    ; Тонкая полоска для пустой части
            'face `(:foreground ,category-color)))
          
          (insert 
           (propertize (format " %.1f%%" progress-percent)
                       'face `(:foreground ,category-color :weight bold)))
          
          ;; Информация о наградах
          (insert 
           (format "\n    Награда за задачу: +%d XP, +%d 🪙\n\n"
                   (plist-get base-rewards :xp)
                   (plist-get base-rewards :gold)))))
      
      ;; Общий прогресс
      (let* ((overall-progress (if (> total-tasks 0) 
                                 (/ (* completed-tasks 100.0) total-tasks)
                               0))
             (quest-bar-width 30)
             (filled-length (round (* quest-bar-width (/ overall-progress 100.0))))
             (empty-length (- quest-bar-width filled-length)))
        
        (insert 
         (propertize "\n## Общий прогресс\n" 
                     'face '(:foreground "#2196F3" :weight bold)))
        
        (insert "  ")
        (insert 
         (propertize (format "%d/%d задач " completed-tasks total-tasks)
                     'face '(:foreground "#333333")))
        
        ;; Цвет прогресс-бара зависит от процента выполнения
        (let ((progress-color
               (cond
                ((>= overall-progress 80) "#4CAF50") ; Зеленый для высокого прогресса
                ((>= overall-progress 50) "#FFA726") ; Оранжевый для среднего
                (t "#FF7043"))))                     ; Красноватый для низкого
          
          (insert 
           (propertize 
            (concat 
             (make-string filled-length ?▰)
             (make-string empty-length ?▱))
            'face `(:foreground ,progress-color)))
          
          (insert 
           (propertize (format " %.1f%%\n" overall-progress)
                       'face `(:foreground ,progress-color :weight bold))))))))
