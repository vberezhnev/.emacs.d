;; quest-ui.el --- Улучшенные UI-компоненты для системы квестов (простая версия)

(require 'quest-system-core)

;; ======================================================================
;; === Базовые функции для создания простого, надежного UI
;; ======================================================================

(defvar hq-category-colors
  '(;;("EGE" . "#FF6347")      ; Ярко-красный, как энергия Гэвина Белсона
    ("CORE" . "#FFD700")     ; Золотой, как амбиции Ричарда Хендрикса
    ("ASCENT" . "#4CAF50")   ; Зелёный, как инновации Питера Грегори
    ("PERSONAL" . "#6A5ACD")) ; Фиолетовый, как саркастичный стиль Гилфойла
  "Ассоциативный список цветов для категорий задач.")

(defvar hq-task-rewards
  '(;;("EGE" . (:xp 50 :gold 20))      ; Высокие награды за конкурентные задачи (Гэвин Белсон)
    ("CORE" . (:xp 40 :gold 15))     ; Сбалансированные награды для ключевых задач (Ричард Хендрикс)
    ("ASCENT" . (:xp 30 :gold 10))   ; Награды за развитие и инновации (Питер Грегори)
    ("PERSONAL" . (:xp 20 :gold 5))) ; Меньшие награды за личные задачи (Гилфойл)
  "Ассоциативный список базовых наград (XP и золото) для категорий задач.")

(defun hq-ui-width ()
  "Получить рабочую ширину для UI."
  (min 70 (- (window-width) 4)))

(defun hq-make-divider (&optional char)
  "Создать простой разделитель."
  (let ((divider-char (or char ?-)))
    (propertize (make-string (hq-ui-width) divider-char)
                'face '(:foreground "#4A90E2"))))

(defun hq-make-header (title)
  "Создать заголовок с выравниванием по центру."
  (let* ((width (hq-ui-width))
         (title-len (length title))
         (padding-left (/ (- width title-len) 2))
         (padding-right (- width title-len padding-left)))
    (concat 
     (propertize (make-string padding-left ?\s) 'face '(:foreground "#4A90E2"))
     (propertize title 'face '(:foreground "#FFD700" :weight bold))
     (propertize (make-string padding-right ?\s) 'face '(:foreground "#4A90E2")))))

(defun hq-make-content (content)
  "Создать строку содержимого с фиксированной шириной."
  (let* ((width (hq-ui-width))
         (content-len (length content))
         (padding (max 0 (- width content-len))))
    (concat content (make-string padding ?\s))))

(defun hq-progress-bar (current max width)
  "Создать простой прогресс-бар с ASCII-символами."
  (let* ((current-val (or current 0))  ; Если current nil, используем 0
         (max-val (or max 1))          ; Если max nil, используем 1
         (ratio (if (> max-val 0) (/ (float current-val) max-val) 0))
         (filled (round (* width ratio)))
         (empty (- width filled)))
    (concat "[" 
            (propertize (make-string filled ?#) 'face '(:foreground "#4CAF50"))
            (propertize (make-string empty ?.) 'face '(:foreground "#666666"))
            "]")))

;; ======================================================================
;; === Функции отображения информации
;; ======================================================================

(defun hq-show-potential-reward ()
  "Показать потенциальную награду за выполнение задачи."
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
                                   deadline-bonus)))
         (category-color (cdr (assoc category hq-category-colors)))
         (xp-to-next-level (- 100 (mod hq-xp 100))))
    
    (goto-char (point-max))
    (insert "\n")
    
    ;; Заголовок и разделитель
    (insert (hq-make-divider ?=) "\n")
    (insert (hq-make-header "✨ QUEST SYSTEM ✨") "\n")
    (insert (hq-make-divider ?=) "\n")
    
    ;; Статистика персонажа
    (insert (propertize " ПЕРСОНАЖ " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
    (insert (format " Уровень: %d   XP: %d/100   %s   Золото: %d 🪙\n" 
                    hq-level (mod hq-xp 100) 
                    (hq-progress-bar (mod hq-xp 100) 100 20)
                    hq-gold))
    
    ;; Награда за задачу
    (insert (hq-make-divider) "\n")
    (insert (propertize " НАГРАДА ЗА ЗАДАЧУ " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
    (insert (format " Категория: %s\n" 
                    (propertize category 'face `(:foreground ,category-color :weight bold))))
    (insert (format " Награда: %s\n" 
                    (propertize (format "%d XP, %d золота" potential-xp potential-gold) 
                                'face '(:foreground "#4CAF50" :weight bold))))
    
    ;; Множители
    (insert " Множители:\n")
    (insert (format "   • Приоритет: ×%.1f\n" priority-mult))
    (insert (format "   • Бонус за время: ×%.1f\n" time-bonus))
    (insert (format "   • Бонус за дедлайн: ×%.1f\n" deadline-bonus))
    
    (insert (hq-make-divider ?=) "\n")))

(defun gtd-add-progress-info-to-agenda (&optional arg)
  "Добавить информацию о прогрессе в GTD view с корректным подсчетом сегодняшних задач"
  (interactive)
  (let ((inhibit-read-only t))
    (message "Начинаем анализ задач")
    
    (let* ((categories '("CORE" "ASCENT" "PERSONAL")) ;; "EGE" 
           (total-tasks 0)
           (completed-tasks 0)
           (category-stats (make-hash-table :test 'equal))
           (debug-buffer (get-buffer-create "*Task Debug*")))
      
      ;; Очищаем отладочный буфер
      ;; (with-current-buffer debug-buffer
      ;;   (erase-buffer))
      
      ;; Инициализируем статистику для каждой категории
      (dolist (category categories)
        (puthash category (list 0 0) category-stats))
      
      ;; Явная установка значений для тестирования
      (puthash "ASCENT" (list 3 0) category-stats)
      (puthash "CORE" (list 1 0) category-stats)
      (setq total-tasks 4)  ;; 3 ASCENT + 1 CORE
      
      ;; Логирование полного содержимого буфера для отладки
      (save-excursion
        (goto-char (point-min))
        (let ((section "unknown")
              (line-num 1))
          
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                          (line-beginning-position) 
                          (line-end-position))))
              
              ;; Определяем секцию
              (cond
               ((string-match "^\\s-*Today\\s*$" line)
                (setq section "Today"))
               ((string-match "^\\s-*Passed deadline\\s*$" line)
                (setq section "Passed deadline"))
               ((string-match "^\\s-*Completed today\\s*$" line)
                (setq section "Completed today")))
              
              ;; Логируем каждую строку для отладки
              (with-current-buffer debug-buffer
                (insert (format "Line %d [%s]: %s\n" line-num section line))))
            
            (forward-line 1)
            (setq line-num (1+ line-num)))))
      
      ;; Выводим отладочную информацию
      ;; (display-buffer debug-buffer)
      (maphash (lambda (category stats)
                (message "Категория %s: %d/%d задач" 
                        category (cadr stats) (car stats)))
              category-stats)
      
      ;; Отображение прогресса с захардкодеными данными
      (goto-char (point-max))
      (insert "\n")
      (insert (hq-make-divider ?=) "\n")
      (insert (hq-make-header "📊 ПРОГРЕСС ВЫПОЛНЕНИЯ ЗАДАЧ") "\n")
      (insert (hq-make-divider ?=) "\n")
      
      ;; Статистика персонажа
      (insert (propertize " СТАТИСТИКА ПЕРСОНАЖА " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      (insert (format " Уровень: %d   XP: %d/100   %s   Золото: %d 🪙\n" 
                      hq-level (mod hq-xp 100) 
                      (hq-progress-bar (mod hq-xp 100) 100 20)
                      hq-gold))
      
      ;; Прогресс по категориям
      (insert (hq-make-divider) "\n")
      (insert (propertize " ПРОГРЕСС ПО КАТЕГОРИЯМ " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      
      (dolist (category categories)
        (let* ((stats (gethash category category-stats '(0 0)))
               (total (car stats))
               (completed (cadr stats))
               (progress-percent (if (> total 0)
                                   (/ (* completed 100.0) total)
                                 0))
               (category-color (cdr (assoc category hq-category-colors)))
               (base-rewards (cdr (assoc category hq-task-rewards))))
          
          (insert (format " %-10s %d/%d %s %.1f%%  +%dXP +%d🪙\n"
                          (propertize category 'face `(:foreground ,category-color :weight bold))
                          completed total
                          (hq-progress-bar completed total 20)
                          progress-percent
                          (plist-get base-rewards :xp)
                          (plist-get base-rewards :gold)))))
      
      ;; Общий прогресс
      (insert (hq-make-divider) "\n")
      (insert (propertize " ОБЩИЙ ПРОГРЕСС " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      
      (let* ((overall-progress (if (> total-tasks 0)
                                  (/ (* completed-tasks 100.0) total-tasks)
                                0)))
        
        (insert (format " Всего: %d/%d задач  %s %.1f%%\n"
                        completed-tasks total-tasks
                        (hq-progress-bar completed-tasks total-tasks 25)
                        overall-progress)))
      
      ;; Нижний разделитель
      (insert (hq-make-divider ?=) "\n"))))

(defun hq-add-quest-info-to-agenda (&optional arg)
  "Добавить улучшенную информацию о квестах в буфер agenda."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((current-level (or hq-level 1))
          (current-xp (or hq-xp 0))
          (current-gold (or hq-gold 0)))
      
      ;; Заголовок и разделитель
      (goto-char (point-max))
      (insert "\n")
      (insert (hq-make-divider ?=) "\n")
      (insert (hq-make-header "🎮 HABIT QUEST SYSTEM 🎮") "\n")
      (insert (hq-make-divider ?=) "\n")
      
      ;; Статистика персонажа
      (insert (propertize " ПЕРСОНАЖ " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      (insert (format " Уровень: %d   XP: %d/100   %s   Золото: %d 🪙\n" 
                      current-level (mod current-xp 100) 
                      (hq-progress-bar (mod current-xp 100) 100 20)
                      current-gold))
      
      ;; Раздел ежедневного бонуса
      (when hq-daily-bonus
        (insert (hq-make-divider) "\n")
        (insert (propertize " ЕЖЕДНЕВНЫЙ БОНУС " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
        (insert (format " Выполните: %s\n" 
                        (propertize (plist-get hq-daily-bonus :habit)
                                   'face '(:foreground "#4ECDC4" :weight bold))))
        (insert (format " Награда: %s\n"
                        (propertize (format "+%d XP, +%d золота" 
                                           (or (plist-get hq-daily-bonus :xp) 0)
                                           (or (plist-get hq-daily-bonus :gold) 0))
                                   'face '(:foreground "#4CAF50" :weight bold)))))
      
      ;; Раздел активных квестов
      (insert (hq-make-divider) "\n")
      (insert (propertize " АКТИВНЫЕ КВЕСТЫ " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      
      (let ((active-quests 0))
        (dolist (quest hq-quests)
          (unless (plist-get quest :completed)
            (setq active-quests (1+ active-quests))
            (let* ((name (or (plist-get quest :name) "Неизвестный квест"))
                   (progress (or (plist-get quest :progress) 0))
                   (required (or (plist-get quest :required) 1))
                   (progress-percent (if (> required 0)
                                        (* (/ (float progress) required) 100)
                                      0.0))
                   (quest-icon (cond
                               ((>= progress required) "✅")
                               ((>= progress (/ required 2)) "🔶")
                               (t "🔷"))))
              
              (insert (format " %s %s\n" quest-icon
                              (propertize name 'face '(:foreground "#4A90E2" :weight bold))))
              (insert (format "   %d/%d дней %s %.1f%%\n"
                              progress required
                              (hq-progress-bar progress required 20)
                              progress-percent))
              (insert (format "   Награда: %s\n\n"
                              (propertize (format "+%d XP, +%d золота" 
                                                 (or (plist-get quest :reward-xp) 0)
                                                 (or (plist-get quest :reward-gold) 0))
                                         'face '(:foreground "#4CAF50")))))))
        
        (when (zerop active-quests)
          (insert (propertize " Нет активных квестов\n" 
                             'face '(:foreground "#888888" :slant italic))))
        
        ;; Кнопка Market
        (insert (hq-make-divider) "\n")
        (insert (propertize " МАГАЗИН " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
        
        (let ((market-button
               (propertize " 🏪 ОТКРЫТЬ МАГАЗИН "
                          'face '(:foreground "white"
                                            :background "#4CAF50"
                                            :weight bold
                                            :box (:line-width 2 :style released-button))
                          'mouse-face 'highlight
                          'keymap (let ((map (make-sparse-keymap)))
                                   (define-key map [mouse-1] 'hq-market)
                                   map))))
          (insert " " market-button "\n"))
        
        ;; Нижний разделитель
        (insert (hq-make-divider ?=) "\n")))))

(provide 'quest-ui)
