;;; habit-quest.el --- Quest system for org-habit -*- lexical-binding: t; -*-

(require 'quest-system-core)
(require 'widget)
(require 'wid-edit)

;; Базовые показатели персонажа
(defvar hq-xp 0 "Опыт персонажа")
(defvar hq-level 1 "Уровень персонажа")
(defvar hq-gold 0 "Золото персонажа")

;; Список квестов
(defvar hq-quests
  '((:id 1 :name "Путь к осознанности" 
     :description "Выполните все три медитации 5 дней подряд" 
     :habits ("🎯‍ - Утренняя медитация" "🌟️ Дневная медитация" "🌿 - Вечерняя медитация")
     :required 5 :progress 0 :completed nil
     :reward-xp 100 :reward-gold 50)
    
    (:id 2 :name "Железная дисциплина" 
     :description "Просыпайтесь в 05:30 7 дней подряд" 
     :habits ("⏰ - Проснуться в 05;30") 
     :required 7 :progress 0 :completed nil
     :reward-xp 150 :reward-gold 75)
    
    (:id 3 :name "Энергетический баланс" 
     :description "Выпивайте 2 литра воды и делайте 10к шагов 10 дней подряд" 
     :habits ("💧 - 2 литра воды" "🚶 - 10к шагов") 
     :required 10 :progress 0 :completed nil
     :reward-xp 200 :reward-gold 100)
     
    (:id 4 :name "Фокус на учебе" 
     :description "Готовьтесь к ЕГЭ 5 дней подряд" 
     :habits ("📝 - ЕГЭ") 
     :required 5 :progress 0 :completed nil
     :reward-xp 120 :reward-gold 60)
     
    (:id 5 :name "Режим бодрости" 
     :description "Принимайте контрастный душ 7 дней подряд" 
     :habits ("🚿 - Контрастный душ") 
     :required 7 :progress 0 :completed nil
     :reward-xp 140 :reward-gold 70))
  "Список квестов")

;; Система магазина
(defvar hq-shop-items
  '(("Дополнительный час отдыха" . 50)
    ("Поход в компушку" . 500)
    ("Просмотр одной серии" . 60)
    ("Просмотр одного фильма" . 100)
    ("Включить музыку" . 10))
  "Товары в магазине и их стоимость.")

(defvar hq-inventory '()
  "Предметы в инвентаре игрока.")

;; Система бонусных заданий
(defvar hq-daily-bonus nil
  "Текущее ежедневное бонусное задание.")

(defvar hq-last-bonus-date nil
  "Дата последнего обновления бонусного задания.")

;; Функции для сохранения и загрузки данных
(defun hq-save-data ()
  "Сохранить данные квестовой системы"
  (with-temp-file "~/.emacs.d/habit-quest-data.el"
    (prin1 (list hq-xp 
                hq-level 
                hq-gold 
                hq-quests 
                hq-inventory
                hq-daily-bonus
                hq-last-bonus-date) 
          (current-buffer))))

(defun hq-load-data ()
  "Загрузить данные квестовой системы"
  (when (file-exists-p "~/.emacs.d/habit-quest-data.el")
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/habit-quest-data.el")
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (setq hq-xp (nth 0 data)
              hq-level (nth 1 data)
              hq-gold (nth 2 data)
              hq-quests (nth 3 data))
        (when (> (length data) 4)
          (setq hq-inventory (nth 4 data)))
        (when (> (length data) 5)
          (setq hq-daily-bonus (nth 5 data)))
        (when (> (length data) 6)
          (setq hq-last-bonus-date (nth 6 data)))))))

;; Функция для обновления статистики квестов
(defun hq-update-quest-progress ()
  "Обновить прогресс квестов на основе текущих стриков привычек"
  (interactive)
  ;; Создаем временный хэш для хранения текущих стриков привычек
  (let ((habit-streaks (make-hash-table :test 'equal))
        (agenda-buffer (get-buffer "*Org Agenda*")))
    
    ;; Проверяем, что буфер agenda существует
    (when agenda-buffer
      ;; Собираем информацию о стриках
      (with-current-buffer agenda-buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (get-text-property (point) 'org-habit-p)
              (let* ((marker (get-text-property (point) 'org-hd-marker))
                     (habit-name nil)
                     (streak 0))
                ;; Получаем название привычки
                (when marker
                  (with-current-buffer (marker-buffer marker)
                    (save-excursion
                      (goto-char (marker-position marker))
                      (setq habit-name (org-get-heading t t t t)))))
                
                ;; Получаем стрик из [🔥 N]
                (save-excursion
                  (when (re-search-forward "\\[🔥 \\([0-9]+\\)\\]" (line-end-position) t)
                    (setq streak (string-to-number (match-string 1)))))
                
                ;; Сохраняем стрик
                (when habit-name
                  (puthash habit-name streak habit-streaks))))
            (forward-line 1)))))
    
    ;; Обновляем прогресс квестов
    (dolist (quest hq-quests)
      (unless (plist-get quest :completed)
        (let ((min-streak 999)
              (habits (plist-get quest :habits)))
          ;; Находим минимальный стрик среди связанных привычек
          (dolist (habit habits)
            (let ((streak (gethash habit habit-streaks 0)))
              (when (< streak min-streak)
                (setq min-streak streak))))
          
          ;; Ограничиваем минимальный стрик разумным значением
          (when (= min-streak 999)
            (setq min-streak 0))
          
          ;; Обновляем прогресс квеста
          (setf (plist-get quest :progress) min-streak)
          
          ;; Проверяем завершение квеста
          (when (>= min-streak (plist-get quest :required))
            (setf (plist-get quest :completed) t))))
    
    ;; Сохраняем обновленные данные
    (hq-save-data))))

;; Функция для генерации бонусного задания
(defun hq-generate-daily-bonus ()
  "Генерировать новое ежедневное бонусное задание."
  (interactive)
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (all-habits '()))
    
    ;; Собираем все привычки
    (dolist (quest hq-quests)
      (unless (plist-get quest :completed)
        (dolist (habit (plist-get quest :habits))
          (push habit all-habits))))
    
    ;; Удаляем дубликаты
    (setq all-habits (delete-dups all-habits))
    
    ;; Обновляем задание только если дата изменилась
    (when (or (not hq-last-bonus-date)
              (not (string= current-date hq-last-bonus-date)))
      (if all-habits
          (let* ((selected-habit (nth (random (length all-habits)) all-habits))
                 (bonus-xp 25)
                 (bonus-gold 15))
            (setq hq-daily-bonus (list :habit selected-habit 
                                       :xp bonus-xp 
                                       :gold bonus-gold)))
        (setq hq-daily-bonus nil))
      
      (setq hq-last-bonus-date current-date)
      (hq-save-data))))

;; Функция для проверки бонусного задания
(defun hq-check-daily-bonus (habit-name)
  (when (and hq-daily-bonus 
             (string= habit-name (plist-get hq-daily-bonus :habit)))
    (let ((bonus-xp (plist-get hq-daily-bonus :xp))
          (bonus-gold (plist-get hq-daily-bonus :gold)))
      (hq-add-xp-and-gold bonus-xp bonus-gold)
      (setq hq-daily-bonus nil)
      (hq-save-data))))

(defun hq-add-quest-info-to-agenda (&optional arg)
  "Добавить информацию о квестах в конец agenda"
  (interactive)
  (let ((inhibit-read-only t))
    ;; Добавляем разделитель перед информацией о квестах
    (goto-char (point-max))
    (insert "\n" 
            (propertize "============================\n" 
                        'face '(:foreground "#4A90E2")))
    (insert (propertize "🎮 HABIT QUEST SYSTEM 🎮\n" 
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
    
    ;; Бонусное задание
    (when hq-daily-bonus
      (insert 
       (propertize "🎯 Бонусное задание на сегодня\n" 
                   'face '(:foreground "#2196F3" :weight bold))
       (format "Выполните привычку '%s' для получения бонуса: " 
               (plist-get hq-daily-bonus :habit))
       (propertize (format "%d XP, %d золота\n\n" 
                           (plist-get hq-daily-bonus :xp)
                           (plist-get hq-daily-bonus :gold))
                   'face '(:foreground "#4CAF50"))))
    
    ;; Активные квесты
    (insert 
     (propertize "📜 Активные квесты\n" 
                 'face '(:foreground "#2196F3" :weight bold)))
    
    (let ((active-quests 0)
          (quest-bar-width 20))
      (dolist (quest hq-quests)
        (unless (plist-get quest :completed)
          (setq active-quests (1+ active-quests))
          (let* ((name (plist-get quest :name))
                 (progress (plist-get quest :progress))
                 (required (plist-get quest :required))
                 (progress-percent (if (> required 0) 
                                       (/ (* progress 100.0) required) 
                                     0))
                 (filled-length (round (* quest-bar-width (/ progress-percent 100.0))))
                 (empty-length (- quest-bar-width filled-length)))
            
            (insert 
             (propertize (format "∘ %s\n" name) 
                         'face '(:foreground "#4A90E2")))
             
            (insert 
             (propertize (format "  %d/%d дней " progress required)
                         'face '(:foreground "#333333")))
            
            (insert 
             (propertize 
              (concat 
               (make-string filled-length ?#) 
               (make-string empty-length ?·))
              'face '(:foreground "#4CAF50")) 
             (format " %.1f%%\n\n" progress-percent)))))
      
      (when (zerop active-quests)
        (insert 
         (propertize "  Нет активных квестов\n" 
                     'face '(:foreground "#888888" :slant italic)))))
    ; Кнопка перехода в Habit Market
    (insert "\n")
    (let ((market-button 
           (propertize "[🏪 Открыть Market]" 
                       'face '(:foreground "white" 
                               :background "#4CAF50" 
                               :weight bold 
                               :box (:line-width 2 :style released-button))
                       'mouse-face 'highlight
                       'keymap (let ((map (make-sparse-keymap)))
                                 (define-key map [mouse-1] 'hq-market)
                                 map))))
      (insert market-button "\n"))))

;; Хук для выделения бонусного задания
(defun hq-org-habit-streak-hook ()
  "Хук для добавления в org-habit-streak-count для интеграции с квестовой системой."
  (with-current-buffer "*Org Agenda*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-habit-p)
          (let* ((marker (get-text-property (point) 'org-hd-marker))
                 (habit-name nil))
            (when marker
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (setq habit-name (org-get-heading t t t t)))))
            
            ;; Проверяем, является ли привычка бонусным заданием
            (when (and hq-daily-bonus habit-name 
                       (string= habit-name (plist-get hq-daily-bonus :habit)))
              (save-excursion
                (end-of-line)
                (let ((inhibit-read-only t))
                  (insert " 🌟"))))))
        (forward-line 1)))))

;; Функция для ручного завершения квеста
(defun hq-complete-quest (quest-name)
  "Вручную завершить квест и получить награду"
  (interactive
   (list (completing-read "Выберите квест для завершения: "
                         (mapcar (lambda (quest)
                                   (unless (plist-get quest :completed)
                                     (plist-get quest :name)))
                                 hq-quests)
                         nil t)))
  
  ;; Находим выбранный квест
  (let ((quest nil))
    (dolist (q hq-quests)
      (when (and (not (plist-get q :completed))
                 (string= (plist-get q :name) quest-name))
        (setq quest q)))
    
    ;; Если квест найден, завершаем его
    (if quest
        (progn
          ;; Отмечаем как завершенный
          (setf (plist-get quest :completed) t)
          
          ;; Начисляем награды
          (let ((reward-xp (plist-get quest :reward-xp))
                (reward-gold (plist-get quest :reward-gold)))
            (setq hq-xp (+ hq-xp reward-xp))
            (setq hq-gold (+ hq-gold reward-gold))
            
            ;; Проверяем повышение уровня
            (let ((new-level (1+ (/ hq-xp 100))))
              (when (> new-level hq-level)
                (setq hq-level new-level)
                (message "Уровень повышен! Теперь вы уровня %d!" hq-level)))
            
            (message "Квест завершен: %s! +%d XP, +%d золота!"
                     quest-name reward-xp reward-gold)))
        
        (message "Квест не найден или уже завершен")))
    
    ;; Сохраняем данные
    (hq-save-data))

;; Функция для форматирования товаров магазина с ценой
(defun hq-format-shop-item-with-price (item)
  "Форматирует товар с его ценой для отображения."
  (let ((name (car item))
        (cost (cdr item)))
    (format "%s (стоимость: %d золота)" name cost)))

;; Функции магазина
(defun hq-shop ()
  "Открыть магазин предметов."
  (interactive)
  (with-current-buffer (get-buffer-create "*Habit Shop*")
    (erase-buffer)
    (insert "# 🏪 Магазин наград\n\n")
    (insert (format "У вас: %d золота 🪙\n\n" hq-gold))
    
    (insert "## Доступные награды\n\n")
    (dolist (item hq-shop-items)
      (let ((name (car item))
            (cost (cdr item)))
        (insert (format "- **%s** - %d золота " name cost))
        (if (>= hq-gold cost)
            (insert "✅ *Доступно*")
          (insert "❌ *Недостаточно золота*"))
        (insert "\n")))
    
    (insert "\n## Инвентарь\n\n")
    (if hq-inventory
        (dolist (item hq-inventory)
          (insert (format "- %s\n" item)))
      (insert "Инвентарь пуст.\n"))
    
    (org-mode)
    (display-buffer (current-buffer))))

(defun hq-format-shop-item (item)
  "Форматирует товар с его ценой."
  (let ((name (car item))
        (cost (cdr item)))
    (format "%s (стоимость: %d золота)" name cost)))

(defun hq-buy-item ()
  "Купить предмет из магазина."
  (interactive)
  (let* ((item-names (mapcar #'hq-format-shop-item-with-price hq-shop-items))
         (selected-formatted (completing-read "Выберите предмет: " item-names nil t))
         ;; Извлекаем оригинальное название товара
         (item-name (car (split-string selected-formatted " (стоимость:")))
         (cost (cdr (assoc-string item-name hq-shop-items))))
    (if (and cost (>= hq-gold cost))
        (progn
          (setq hq-gold (- hq-gold cost))
          (push item-name hq-inventory)
          (hq-save-data)
          (message "Вы купили %s за %d золота." item-name cost))
      (message "Недостаточно золота для покупки %s." item-name))))

(defun hq-use-item ()
  "Использовать предмет из инвентаря."
  (interactive)
  (if hq-inventory
      (let ((item (completing-read "Выберите предмет для использования: " hq-inventory nil t)))
        (setq hq-inventory (delete item hq-inventory))
        (hq-save-data)
        (message "Вы использовали %s. Приятного отдыха!" item))
    (message "Ваш инвентарь пуст.")))

;; Функция для отображения детальной информации о квестах
;; (defun hq-display-quests ()
;;   "Отобразить подробную информацию о квестах"
;;   ;; (interactive)
  
;;   ;; ;; Сначала открываем agenda
;;   ;; (org-agenda nil "x")
  
;;   ;; ;; Ждем немного для загрузки agenda и затем запускаем обновление
;;   ;; (run-with-timer 0.5 nil 
;;                   (lambda ()
;;                     ;; Обновляем прогресс
;;                     (when (get-buffer "*Org Agenda*")
;;                       (let ((habit-streaks (make-hash-table :test 'equal)))
                        
;;                         ;; Собираем информацию о стриках
;;                         (with-current-buffer "*Org Agenda*"
;;                           (save-excursion
;;                             (goto-char (point-min))
;;                             (while (not (eobp))
;;                               (when (get-text-property (point) 'org-habit-p)
;;                                 (let* ((marker (get-text-property (point) 'org-hd-marker))
;;                                        (habit-name nil)
;;                                        (streak 0))
;;                                   ;; Получаем название привычки
;;                                   (when marker
;;                                     (with-current-buffer (marker-buffer marker)
;;                                       (save-excursion
;;                                         (goto-char (marker-position marker))
;;                                         (setq habit-name (org-get-heading t t t t)))))
                                  
;;                                   ;; Получаем стрик из [🔥 N]
;;                                   (save-excursion
;;                                     (when (re-search-forward "\\[🔥 \\([0-9]+\\)\\]" (line-end-position) t)
;;                                       (setq streak (string-to-number (match-string 1)))))
                                  
;;                                   ;; Сохраняем стрик
;;                                   (when habit-name
;;                                     (puthash habit-name streak habit-streaks))))
;;                               (forward-line 1))))
                        
;;                         ;; Обновляем прогресс квестов
;;                         (dolist (quest hq-quests)
;;                           (unless (plist-get quest :completed)
;;                             (let ((min-streak 999)
;;                                   (habits (plist-get quest :habits)))
;;                               ;; Находим минимальный стрик среди связанных привычек
;;                               (dolist (habit habits)
;;                                 (let ((streak (gethash habit habit-streaks 0)))
;;                                   (when (< streak min-streak)
;;                                     (setq min-streak streak))))
                              
;;                               ;; Ограничиваем минимальный стрик разумным значением
;;                               (when (= min-streak 999)
;;                                 (setq min-streak 0))
                              
;;                               ;; Обновляем прогресс квеста
;;                               (setf (plist-get quest :progress) min-streak))))
                        
;;                         ;; Отображаем детальную информацию
;;                         (with-current-buffer (get-buffer-create "*Habit Quests*")
;;                           (erase-buffer)
;;                           (insert "# 📜 Система квестов привычек\n\n")
                          
;;                           ;; Информация о персонаже
;;                           (insert "## Статистика персонажа\n\n")
;;                           (insert (format "Уровень: %d\n" hq-level))
;;                           (insert (format "Опыт: %d/%d\n" (mod hq-xp 100) 100))
;;                           (insert (format "Золото: %d\n\n" hq-gold))
                          
;;                           ;; Бонусное задание
;;                           (when hq-daily-bonus
;;                             (insert "## 🎯 Бонусное задание на сегодня\n\n")
;;                             (insert (format "Выполните привычку '%s' для получения бонуса: %d XP, %d золота\n\n"
;;                                           (plist-get hq-daily-bonus :habit)
;;                                           (plist-get hq-daily-bonus :xp)
;;                                           (plist-get hq-daily-bonus :gold))))
                          
;;                           ;; Активные квесты
;;                           (insert "## Активные квесты\n\n")
;;                           (let ((active-quests 0))
;;                             (dolist (quest hq-quests)
;;                               (unless (plist-get quest :completed)
;;                                 (setq active-quests (1+ active-quests))
;;                                 (insert (format "### %s\n" (plist-get quest :name)))
;;                                 (insert (format "%s\n\n" (plist-get quest :description)))
                                
;;                                 ;; Связанные привычки
;;                                 (insert "**Привычки:**\n")
;;                                 (dolist (habit (plist-get quest :habits))
;;                                   (insert (format "- %s (Стрик: %d)\n" 
;;                                                  habit
;;                                                  (gethash habit habit-streaks 0))))
;;                                 (insert "\n")
                                
;;                                 ;; Прогресс квеста
;;                                 (let* ((progress (plist-get quest :progress))
;;                                        (required (plist-get quest :required))
;;                                        (progress-percent (/ (* progress 100.0) required)))
;;                                   (insert (format "**Прогресс:** %d/%d дней (%.1f%%)\n"
;;                                                  progress required progress-percent))
;;                                   (insert "[")
;;                                   (insert (make-string (round (/ (* 20 progress) required)) ?#))
;;                                   (insert (make-string (- 20 (round (/ (* 20 progress) required))) ?\s))
;;                                   (insert "]\n\n"))

;;                                 ;; Награды
;;                                 (insert (format "**Награды:** %d XP, %d золота\n\n"
;; 																								(plist-get quest :reward-xp)
;; 																								(plist-get quest :reward-gold)))))

;;                             (when (zerop active-quests)
;;                               (insert "У вас нет активных квестов.\n\n")))

;;                           ;; Завершенные квесты
;;                           (insert "## Завершенные квесты\n\n")
;;                           (let ((completed-quests 0))
;;                             (dolist (quest hq-quests)
;;                               (when (plist-get quest :completed)
;;                                 (setq completed-quests (1+ completed-quests))
;;                                 (insert (format "- **%s**: %s\n"
;; 																								(plist-get quest :name)
;; 																								(plist-get quest :description)))))

;;                             (when (zerop completed-quests)
;;                               (insert "Вы еще не завершили ни одного квеста.\n")))

;;                           (org-mode)
;;                           (display-buffer (current-buffer)))

;;                         ;; Сохраняем обновленные данные
;;                         (hq-save-data)

;;                         ;; Закрываем agenda buffer
;;                         (kill-buffer "*Org Agenda*"))))) ;;)

;; Функция для отображения quest view (исправленная)
(defun hq-habits-quest-view ()
  "Отобразить agenda с информацией о квестах"
  (interactive)
  
  ;; Генерируем бонусное задание перед открытием view
  (hq-generate-daily-bonus)
  
  ;; Вызываем оригинальный Habits view
  (org-agenda nil "x")
  
  ;; Обновляем информацию о квестах через таймер
  (run-with-timer 0.5 nil
                  (lambda ()
                    (when (get-buffer "*Org Agenda*")
                      ;; Обновляем прогресс
                      (hq-update-quest-progress)
                      
                      ;; Добавляем информацию о квестах
                      (with-current-buffer "*Org Agenda*"
                        (save-excursion
                          (let ((inhibit-read-only t))
                            (hq-add-quest-info-to-agenda))))))))

(defvar hq-daily-bonus nil
  "Текущее ежедневное бонусное задание.")

(defvar hq-last-bonus-date nil
  "Дата последнего обновления бонусного задания.")

(defun hq-generate-daily-bonus ()
  "Генерировать новое ежедневное бонусное задание."
  (interactive)
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (all-habits '()))

    ;; Собираем все привычки
    (dolist (quest hq-quests)
      (unless (plist-get quest :completed)
        (dolist (habit (plist-get quest :habits))
          (push habit all-habits))))

    ;; Удаляем дубликаты
    (setq all-habits (delete-dups all-habits))

    ;; Обновляем задание только если дата изменилась
    (when (or (not hq-last-bonus-date)
              (not (string= current-date hq-last-bonus-date)))
      (if all-habits
          (let* ((selected-habit (nth (random (length all-habits)) all-habits))
                 (bonus-xp 40)
                 (bonus-gold 30))
            (setq hq-daily-bonus (list :habit selected-habit
                                       :xp bonus-xp
                                       :gold bonus-gold)))
        (setq hq-daily-bonus nil))

      (setq hq-last-bonus-date current-date)
      (hq-save-data))))

(defun hq-check-daily-bonus (habit-name)
  "Проверить, является ли привычка бонусным заданием."
  (when (and hq-daily-bonus
             (string= habit-name (plist-get hq-daily-bonus :habit)))
    (let ((bonus-xp (plist-get hq-daily-bonus :xp))
          (bonus-gold (plist-get hq-daily-bonus :gold)))
      (setq hq-xp (+ hq-xp bonus-xp))
      (setq hq-gold (+ hq-gold bonus-gold))
      (message "Бонусное задание выполнено! +%d XP, +%d золота!"
               bonus-xp bonus-gold)
      (setq hq-daily-bonus nil)
      (hq-save-data))))

;; Интерактивный магазин с кнопками

(defvar hq-market-buffer-name "*Habit Market*"
  "Имя буфера для магазина.")

(defun hq-market ()
  "Открыть интерактивный магазин с кнопками покупки и использования."
  (interactive)
  (let ((buffer (get-buffer-create hq-market-buffer-name)))
    (switch-to-buffer buffer)
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    
    ;; Стильное оформление заголовка
    (widget-insert (propertize "🏪 Магазин наград\n" 
                               'face '(:foreground "#4A90E2" :weight bold :height 1.2)))
    (widget-insert (propertize (format "У вас: %d золота 🪙\n" hq-gold)
                               'face '(:foreground "#FFD700" :weight bold)))
    (widget-insert (propertize "============================\n\n" 
                               'face '(:foreground "#4A90E2")))
    
    ;; Список товаров с улучшенным форматированием
    (widget-insert (propertize "## Доступные награды\n" 
                               'face '(:foreground "#4A90E2" :weight bold)))
    (dolist (item hq-shop-items)
      (let* ((name (car item))
             (cost (cdr item))
             (in-inventory (member name hq-inventory))
             (name-width 35)  ; Увеличенная ширина для красивого выравнивания
             (formatted-name (format (format "%%-%ds" name-width) name)))
        
        (widget-insert 
         (propertize 
          (format "- %s - " formatted-name)
          'face '(:foreground "#333333")))
        
        (widget-insert 
         (propertize 
          (format "🪙%-5d " cost)
          'face '(:foreground "#FFD700" :weight bold)))
        
        (cond 
         ;; Если товар есть в инвентаре - кнопка "Использовать"
         (in-inventory
          (widget-create 'push-button 
                         :notify (lambda (&rest _) 
                                   (hq-use-market-item name))
                         :face '(:foreground "white" :background "#4CAF50" :weight bold)
                         "Использовать"))
         
         ;; Если достаточно золота - кнопка "Купить"
         ((>= hq-gold cost)
          (widget-create 'push-button 
                         :notify (lambda (&rest _) 
                                   (hq-buy-market-item name cost))
                         :face '(:foreground "white" :background "#2196F3" :weight bold)
                         "Купить"))
         
         ;; Недостаточно золота
         (t 
          (widget-insert 
           (propertize "❌ Недостаточно золота" 
                       'face '(:foreground "#FF5722")))))
        
        (widget-insert "\n")))
    
    ;; Секция инвентаря
    (widget-insert "\n")
    (widget-insert (propertize "## Ваш инвентарь\n" 
                               'face '(:foreground "#4A90E2" :weight bold)))
    (if hq-inventory
        (dolist (item hq-inventory)
          (widget-insert 
           (propertize (format "- %s\n" item) 
                       'face '(:foreground "#333333"))))
      (widget-insert 
       (propertize "Инвентарь пуст.\n" 
                   'face '(:foreground "#888888" :slant italic))))
    
    (use-local-map widget-keymap)
    (local-set-key "q" 'kill-this-buffer)
    
    (widget-setup)
    (goto-char (point-min))))

(defun hq-buy-market-item (name cost)
  "Купить товар из магазина."
  (when (>= hq-gold cost)
    (setq hq-gold (- hq-gold cost))
    (push name hq-inventory)
    (hq-save-data)
    (message "Вы купили %s за %d золота." name cost)
    (hq-market)))  ; Обновляем вид магазина

(defun hq-use-market-item (name)
  "Использовать товар из инвентаря."
  (setq hq-inventory (delete name hq-inventory))
  (hq-save-data)
  (message "Вы использовали %s. Приятного отдыха!" name)
  (hq-market))  ; Обновляем вид магазина

;; Привязка к удобной комбинации клавиш
(global-set-key (kbd "C-c m") 'hq-market)

;; Настройка
(defun hq-setup ()
  "Настроить квестовую систему привычек"
  (interactive)

  ;; Загружаем сохраненные данные
  (hq-load-data)

  ;; Привязка клавиш
  (global-set-key (kbd "C-c q") 'hq-habits-quest-view)
  (global-set-key (kbd "C-c C-q") 'hq-display-quests)

  (message "Квестовая система для org-habit настроена!"))

;; Устанавливаем систему
(hq-setup)

;;; habit-quest.el ends here
