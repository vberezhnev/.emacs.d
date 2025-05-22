;;; habit-quest.el --- Quest system for org-habit -*- lexical-binding: t; -*-

(require 'quest-system-core)
(require 'quest-ui)
(require 'widget)
(require 'wid-edit)
(require 'market)
;; (require 'penalties)

;; Базовые показатели персонажа
(defvar hq-xp 0 "Опыт персонажа")
(defvar hq-level 1 "Уровень персонажа")
(defvar hq-gold 0 "Золото персонажа")

;; Список квестов
(defvar hq-quests
  '((:id 1 :name "Путь к осознанности"
				 :description "Выполните все три медитации 5 дней подряд"
				 :habits ("🎯‍ - Утренняя медитация" "🌟️ - Дневная медитация" "🌿 - Вечерняя медитация")
				 :required 5 :progress 0 :completed nil
				 :reward-xp 200 :reward-gold 100)

    (:id 2 :name "Железная дисциплина"
				 :description "Просыпайтесь в 05:30 7 дней подряд"
				 :habits ("⏰ - Проснуться в 05;30")
				 :required 7 :progress 0 :completed nil
				 :reward-xp 300 :reward-gold 150)

    (:id 3 :name "Энергетический баланс"
				 :description "Выпивайте 2 литра воды и делайте 10к шагов 10 дней подряд"
				 :habits ("💧 - 2 литра воды" "🚶 - 10к шагов")
				 :required 10 :progress 0 :completed nil
				 :reward-xp 400 :reward-gold 200)

    (:id 4 :name "Фокус на учебе"
				 :description "Готовьтесь к ЕГЭ 5 дней подряд"
				 :habits ("📝 - ЕГЭ")
				 :required 5 :progress 0 :completed nil
				 :reward-xp 400 :reward-gold 400)

    (:id 5 :name "Режим бодрости"
				 :description "Принимайте контрастный душ 7 дней подряд"
				 :habits ("🚿 - Контрастный душ")
				 :required 7 :progress 0 :completed nil
				 :reward-xp 140 :reward-gold 70))
  "Список квестов")

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

;; (3566 36 319 ((:id 1 :name "Путь к осознанности" :description "Выполните все три медитации 5 дней подряд" :habits ("🎯‍ - Утренняя медитация" "🌟️ - Дневная медитация" "🌿 - Вечерняя медитация") :required 5 :progress 0 :completed nil :reward-xp 100 :reward-gold 50) (:id 2 :name "Железная дисциплина" :description "Просыпайтесь в 05:30 7 дней подряд" :habits ("⏰ - Проснуться в 05;30") :required 7 :progress 7 :completed nil :reward-xp 150 :reward-gold 75) (:id 3 :name "Энергетический баланс" :description "Выпивайте 2 литра воды и делайте 10к шагов 10 дней подряд" :habits ("💧 - 2 литра воды" "🚶 - 10к шагов") :required 10 :progress 0 :completed nil :reward-xp 200 :reward-gold 100) (:id 4 :name "Фокус на учебе" :description "Готовьтесь к ЕГЭ 5 дней подряд" :habits ("📝 - ЕГЭ") :required 5 :progress 0 :completed nil :reward-xp 120 :reward-gold 60) (:id 5 :name "Режим бодрости" :description "Принимайте контрастный душ 7 дней подряд" :habits ("🚿 - Контрастный душ") :required 7 :progress 0 :completed nil :reward-xp 140 :reward-gold 70)) nil (:habit "🌟️ - Дневная медитация" :xp 40 :gold 30) "2025-03-07")

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
              hq-quests (nth 3 data)
              hq-inventory (nth 4 data)
              hq-daily-bonus (nth 5 data)
              hq-last-bonus-date (nth 6 data)
              hq-penalty-history (nth 7 data))))))

(defun hq-calculate-combined-streak (habits habit-stats)
  "Подсчитать количество последовательных дней, когда все привычки были выполнены.
HABITS - список привычек для проверки
HABIT-STATS - хеш-таблица с данными о привычках"
  (let ((streaks-data nil))
    ;; Собираем состояния всех привычек
    (dolist (habit habits)
      (when-let ((habit-data (gethash habit habit-stats)))
        (push (cdr habit-data) streaks-data)))
    
    ;; Если получили данные для всех привычек
    (when (= (length streaks-data) (length habits))
      (let ((combined-streak 0)
            (day-index 0)
            (continue t))
        ;; Идем по дням, начиная с последнего
        (while (and continue
                    (< day-index (length (car streaks-data))))
          (let ((all-done t))
            ;; Проверяем все привычки в текущий день
            (dolist (habit-state streaks-data)
              (when (and (< day-index (length habit-state))
                        (not (char-equal (aref habit-state day-index) ?●)))
                (setq all-done nil)))
            
            ;; Если все привычки выполнены в этот день
            (if all-done
                (setq combined-streak (1+ combined-streak))
              ;; Если хоть одна не выполнена, прекращаем подсчет
              (setq continue nil)))
          (setq day-index (1+ day-index)))
        combined-streak))))

;; Добавляем глобальную переменную для хранения статистики привычек
(defvar habit-stats (make-hash-table :test 'equal)
  "Хеш-таблица для хранения статистики привычек.
Ключи - названия привычек, значения - пары (стрик . строка-состояния)")

;; Функция для обновления статистики квестов
(defun hq-update-quest-progress ()
  "Обновить прогресс квестов на основе текущих стриков привычек."
  (interactive)
  (clrhash habit-stats)
  
  ;; Собираем информацию о привычках
  (with-current-buffer "*Org Agenda*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-habit-p)
          (let* ((marker (get-text-property (point) 'org-hd-marker))
                 (habit-name nil)
                 (habit-streak 0)
                 (habits-state nil))
            
            ;; Получаем название привычки
            (when marker
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (setq habit-name (org-get-heading t t t t)))))
            
            ;; Получаем актуальные данные о стриках
            (save-excursion
              (end-of-line)
              (when (re-search-backward "\\[🔥 \\([0-9]+\\)\\]" (line-beginning-position) t)
                (setq habit-streak (string-to-number (match-string 1)))
                ;; Также сохраняем состояние привычки
                (when (re-search-backward "\\([○●◎]+\\)" (line-beginning-position) t)
                  (setq habits-state (buffer-substring-no-properties
                                    (match-beginning 1)
                                    (match-end 1)))))
            
            ;; Сохраняем данные привычки
            (when (and habit-name habit-streak)
              (puthash habit-name
                       (cons habit-streak (or habits-state ""))
                       habit-stats)))))
        (forward-line 1)))
    
    ;; Обновляем прогресс квестов
  (dolist (quest hq-quests)
    (unless (plist-get quest :completed)
      (let* ((habits (plist-get quest :habits))
             (required (plist-get quest :required))
             (current-progress 0))
        
        (if (= (length habits) 1)
            ;; Для квестов с одной привычкой
            (let ((habit-data (gethash (car habits) habit-stats)))
              (when habit-data
                (setq current-progress (car habit-data))))
          
          ;; Для квестов с несколькими привычками
          ;; Используем новую функцию подсчета общего стрика
          (setq current-progress 
                (hq-calculate-combined-streak habits habit-stats)))
        
        ;; Обновляем прогресс квеста
        (setf (plist-get quest :progress) current-progress))))
  
    
    ;; Сохраняем обновленные данные
    (hq-save-data)))

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

;; Функция для отображения quest view (исправленная)
(defun hq-habits-quest-view ()
  "Отобразить agenda с информацией о квестах."
  (interactive)
  
  ;; Проверяем, что все необходимые переменные инициализированы
  (unless hq-quests
    (setq hq-quests '()))  ; Инициализируем пустым списком если nil
  
  ;; Генерируем бонусное задание перед открытием view
  (hq-generate-daily-bonus)
  
  ;; Защищенный вызов org-agenda
  (condition-case err
      (org-agenda nil "x")
    (error
     (message "Ошибка при открытии agenda: %s" err)))
  
  ;; Устанавливаем таймер для обновления с проверками
  (run-with-timer 0.5 nil
                  (lambda ()
                    (when (get-buffer "*Org Agenda*")
                      (with-current-buffer "*Org Agenda*"
                        (save-excursion
                          ;; Обновляем прогресс с защитой от ошибок
                          (condition-case nil
                              (progn
                                (hq-update-quest-progress)
                                (let ((inhibit-read-only t))
                                  (hq-add-quest-info-to-agenda)))
                            (error
                             (message "Ошибка при обновлении информации о квестах")))))))))

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

;; Настройка
(defun hq-setup ()
  "Настроить квестовую систему привычек"
  (interactive)

  ;; Загружаем сохраненные данные
  (hq-load-data)


  (message "Квестовая система для org-habit настроена!"))

(defun hq-update-quest-info ()
  "Обновить информацию о квестах в agenda."
  (when (string= (buffer-name) "*Org Agenda*")
    (message "Updating quest progress...")  ; Отладочное сообщение
    (hq-update-quest-progress)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (hq-add-quest-info-to-agenda)))))

  ;; Привязка клавиш
  (global-set-key (kbd "C-c q") 'hq-habits-quest-view)
  ;; (global-set-key (kbd "C-c q") ' hq-update-quest-info)

;; Устанавливаем систему
(hq-setup)

;;; habit-quest.el ends here
