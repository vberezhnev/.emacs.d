(require 'quest-system-core)

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
    (let* ((categories '("CODING" "EGE" "MERITRANK" "PERSONAL"))
           (total-tasks 0)
           (completed-tasks 0)
           (category-stats (make-hash-table :test 'equal)))
      
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
               (empty-length (- quest-bar-width filled-length)))
          
          (insert 
           (propertize (format "∘ %s\n" category) 
                       'face '(:foreground "#4A90E2")))
           
          (insert 
           (propertize (format "  %d/%d задач " completed total)
                       'face '(:foreground "#333333")))
          
          (insert 
           (propertize 
            (concat 
             (make-string filled-length ?#) 
             (make-string empty-length ?·))
            'face '(:foreground "#4CAF50")) 
           (format " %.1f%%\n\n" progress-percent))))
      
      ;; Общий прогресс
      (let* ((overall-progress (if (> total-tasks 0) 
                                   (/ (* completed-tasks 100.0) total-tasks)
                                 0))
             (quest-bar-width 20)
             (filled-length (round (* quest-bar-width (/ overall-progress 100.0))))
             (empty-length (- quest-bar-width filled-length)))
        
        (insert 
         (propertize "## Общий прогресс\n" 
                     'face '(:foreground "#2196F3" :weight bold)))
        
        (insert 
         (propertize (format "  %d/%d задач " completed-tasks total-tasks)
                     'face '(:foreground "#333333")))
        
        (insert 
         (propertize 
          (concat 
           (make-string filled-length ?#) 
           (make-string empty-length ?·))
          'face '(:foreground "#4CAF50")) 
         (format " %.1f%%\n" overall-progress))))))
