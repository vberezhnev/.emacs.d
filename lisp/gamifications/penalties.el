;; penalties.el --- Penalty system for habit quest -*- lexical-binding: t; -*-

(require 'quest-system-core)

;; Базовые штрафы
(defvar hq-penalty-values
  '(;; Штрафы за привычки
    (:id "habit-break" :name "Прерывание стрика" 
     :xp-penalty 15 :gold-penalty 10
     :description "Штраф за прерывание стрика привычки")
    
    (:id "daily-skip" :name "Пропуск дня" 
     :xp-penalty 30 :gold-penalty 20
     :description "Штраф за пропуск всех привычек за день")
    
    ;; Штрафы за задачи
    (:id "deadline-miss" :name "Просрочка дедлайна"
     :xp-penalty 20 :gold-penalty 15
     :description "Штраф за нарушение дедлайна задачи")
    
    (:id "long-deadline-miss" :name "Длительная просрочка"
     :xp-penalty 40 :gold-penalty 30
     :description "Штраф за просрочку более 3 дней")
    
    ;; Штрафы за квесты
    (:id "quest-abandon" :name "Прерывание квеста"
     :xp-penalty 50 :gold-penalty 40
     :description "Штраф за прерывание активного квеста")
    
    (:id "quest-stall" :name "Застой квеста"
     :xp-penalty 25 :gold-penalty 20
     :description "Штраф за отсутствие прогресса в квесте более 5 дней"))
  "Список базовых штрафов системы.")

;; История штрафов
(defvar hq-penalty-history nil
  "История наложенных штрафов.")

;; Хук для проверки штрафов при изменении состояния задачи
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (let ((is-habit (member "habit" (org-get-tags))))
              (when is-habit
                ;; Проверяем штрафы для привычек
                (hq-check-habit-penalties)))
            ;; Проверяем штрафы для дедлайнов
            (hq-check-deadline-penalties)))

;; Хук для проверки штрафов каждый день (можно добавить в cron или другой планировщик)
(defun hq-daily-penalty-check ()
  "Ежедневная проверка штрафов."
  (hq-check-quest-penalties))

(defun hq-apply-penalty (penalty-id &optional multiplier)
  "Применить штраф к пользователю.
PENALTY-ID - идентификатор штрафа
MULTIPLIER - множитель штрафа (по умолчанию 1.0)"
  (let* ((penalty (seq-find (lambda (p) 
                             (string= (plist-get p :id) penalty-id))
                           hq-penalty-values))
         (mult (or multiplier 1.0))
         (xp-penalty (round (* (plist-get penalty :xp-penalty) mult)))
         (gold-penalty (round (* (plist-get penalty :gold-penalty) mult))))
    
    ;; Применяем штраф
    (setq hq-xp (max 0 (- hq-xp xp-penalty)))
    (setq hq-gold (max 0 (- hq-gold gold-penalty)))
    
    ;; Записываем в историю
    (push (list :id penalty-id
                :timestamp (current-time)
                :xp-penalty xp-penalty
                :gold-penalty gold-penalty)
          hq-penalty-history)
    
    ;; Сохраняем данные
    (hq-save-data)
    
    ;; Выводим сообщение
    (message "😢 Получен штраф: %s (-%d XP, -%d золота)"
             (plist-get penalty :name)
             xp-penalty gold-penalty)))

;; Функции проверки и наложения штрафов

(defun hq-check-habit-penalties ()
  "Проверить и наложить штрафы за пропущенные привычки."
  (let ((missed-habits 0)
        (total-habits 0))
    
    ;; Проверяем каждую привычку в agenda
    (with-current-buffer "*Org Agenda*"
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (get-text-property (point) 'org-habit-p)
            (setq total-habits (1+ total-habits))
            (save-excursion
              (when (re-search-forward "\\([xX]\\|-\\)" (line-end-position) t)
                (setq missed-habits (1+ missed-habits)))))
          (forward-line 1))))
    
    ;; Если пропущены все привычки
    (when (and (> total-habits 0)
               (= missed-habits total-habits))
      (hq-apply-penalty "daily-skip"))))

(defun hq-check-deadline-penalties ()
  "Проверить и наложить штрафы за просроченные дедлайны."
  (let ((new-overdue-tasks 0))
    (org-map-entries
     (lambda ()
       (let* ((deadline (org-entry-get nil "DEADLINE"))
              (state (org-get-todo-state))
              (last-penalty-date (org-entry-get nil "LAST_PENALTY")))
         
         ;; Проверяем только если:
         ;; 1. Есть дедлайн
         ;; 2. Задача не завершена
         ;; 3. Штраф ещё не начислялся
         (when (and deadline
                   (not (member state org-done-keywords))
                   (not last-penalty-date))
           
           (let* ((deadline-time (org-time-string-to-time deadline))
                  (days-overdue (/ (float-time
                                  (time-subtract (current-time) deadline-time))
                                 86400)))
             ;; Считаем только недавно просроченные задачи
             (when (and (> days-overdue 0) (< days-overdue 7))
               (setq new-overdue-tasks (1+ new-overdue-tasks))
               ;; Отмечаем, что штраф начислен
               (org-entry-put nil "LAST_PENALTY" 
                            (format-time-string "%Y-%m-%d")))))))
     nil 'agenda)
    
    ;; Применяем один общий штраф за все новые просроченные задачи
    (when (> new-overdue-tasks 0)
      (hq-apply-penalty "deadline-miss" new-overdue-tasks))))

(defun hq-check-quest-penalties ()
  "Проверить и наложить штрафы за квесты."
  (dolist (quest hq-quests)
    (unless (plist-get quest :completed)
      (let ((last-progress (plist-get quest :last-progress-date)))
        (when last-progress
          (let ((days-without-progress
                 (/ (float-time
                     (time-subtract (current-time)
                                  (org-time-string-to-time last-progress)))
                    86400)))
            (when (> days-without-progress 5)
              (hq-apply-penalty "quest-stall"))))))))

;; Функция для просмотра истории штрафов
(defun hq-show-penalty-history ()
  "Показать историю штрафов."
  (interactive)
  (with-current-buffer (get-buffer-create "*Penalty History*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      
      (insert (propertize "📋 История штрафов\n"
                         'face '(:height 1.5 :weight bold)))
      (insert "============================\n\n")
      
      (if hq-penalty-history
          (dolist (penalty (reverse hq-penalty-history))
            (let* ((penalty-def (seq-find (lambda (p) 
                                          (string= (plist-get p :id)
                                                  (plist-get penalty :id)))
                                        hq-penalty-values))
                   (timestamp (format-time-string "%Y-%m-%d %H:%M"
                                                (plist-get penalty :timestamp))))
              (insert 
               (format "%s\n" timestamp)
               (propertize (format "%s\n" (plist-get penalty-def :name))
                          'face '(:weight bold))
               (format "  XP: -%d, Золото: -%d\n\n"
                       (plist-get penalty :xp-penalty)
                       (plist-get penalty :gold-penalty)))))
        (insert (propertize "История штрафов пуста\n"
                           'face '(:slant italic))))
      
      (special-mode)
      (local-set-key "q" 'quit-window))
    (switch-to-buffer (current-buffer))))

(provide 'penalties)
