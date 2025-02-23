;; market.el --- Enhanced market system for habit quest -*- lexical-binding: t; -*-

(require 'quest-system-core)

;; Категории товаров
(defvar hq-market-categories
  '((:id "rest" :name "Отдых" :icon "🌟" :description "Способы отдохнуть и восстановиться")
    (:id "entertainment" :name "Развлечения" :icon "🎮" :description "Развлекательные активности")
    (:id "bonus" :name "Бонусы" :icon "✨" :description "Особые привилегии")
    (:id "rare" :name "Редкие товары" :icon "💎" :description "Специальные предметы"))
  "Категории товаров в магазине.")

;; Расширенный список товаров
(defvar hq-market-items
  '(;; Отдых
    (:id "break-30" :name "Перерыв 30 минут" :cost 30 :category "rest" 
     :description "Короткий перерыв для отдыха"
     :use-message "Отдохните и восстановите силы!")
    (:id "break-60" :name "Час отдыха" :cost 50 :category "rest"
     :description "Полноценный час отдыха"
     :use-message "Целый час для восстановления сил!")
    (:id "nap" :name "Дневной сон" :cost 80 :category "rest"
     :description "15-20 минут сна для восстановления энергии"
     :use-message "Приятного и восстанавливающего сна!")
    
    ;; Развлечения
    (:id "episode" :name "Серия сериала" :cost 60 :category "entertainment"
     :description "Просмотр одной серии любимого сериала"
     :use-message "Приятного просмотра!")
    (:id "movie" :name "Фильм" :cost 100 :category "entertainment"
     :description "Просмотр одного фильма"
     :use-message "Наслаждайтесь фильмом!")
    (:id "gaming" :name "Игровая сессия" :cost 120 :category "entertainment"
     :description "1 час любимых игр"
     :use-message "Веселой игры!")
    (:id "youtube" :name "YouTube Time" :cost 40 :category "entertainment"
     :description "30 минут на YouTube"
     :use-message "Приятного просмотра!")
    
    ;; Бонусы
    (:id "delay-1h" :name "Отсрочка на час" :cost 70 :category "bonus"
     :description "Отложить одну задачу на 1 час"
     :use-message "Задача отложена на час.")
    (:id "music" :name "Музыка во время работы" :cost 30 :category "bonus"
     :description "1 час музыки во время работы"
     :use-message "Наслаждайтесь музыкой!")
    (:id "late-wake" :name "Поздний подъём" :cost 150 :category "bonus"
     :description "Разрешение встать на час позже"
     :use-message "Можете поспать подольше!")
    
    ;; Редкие товары
    (:id "day-off" :name "Выходной день" :cost 500 :category "rare"
     :description "Полный выходной от всех задач"
     :discountable nil ; Нельзя применить скидку
     :use-message "Наслаждайтесь заслуженным выходным!")
    (:id "weekend" :name "Игровые выходные" :cost 1000 :category "rare"
     :description "Полноценные выходные для игр"
     :discountable nil
     :use-message "Веселых игровых выходных!"))
  "Расширенный список товаров в магазине.")

;; Система скидок
(defvar hq-market-discount nil
  "Текущая скидка в магазине (процент).")

(defvar hq-market-discount-duration nil
  "Время действия текущей скидки.")

(defvar hq-market-last-refresh nil
  "Время последнего обновления магазина.")

(defun hq-market-apply-random-discount ()
  "Применить случайную скидку к товарам."
  (let ((discount (nth (random 3) '(10 20 30))))
    (setq hq-market-discount discount)
    (setq hq-market-discount-duration (time-add (current-time) (days-to-time 1)))
    (setq hq-market-last-refresh (current-time))))

(defun hq-market-check-discount ()
  "Проверить и обновить статус скидки."
  (when (and hq-market-discount-duration
             (time-less-p hq-market-discount-duration (current-time)))
    (setq hq-market-discount nil)
    (setq hq-market-discount-duration nil)))

;; Функции управления инвентарем
(defun hq-market-add-to-inventory (item-id)
  "Добавить предмет в инвентарь."
  (push item-id hq-inventory)
  (hq-save-data))

(defun hq-market-remove-from-inventory (item-id)
  "Удалить предмет из инвентаря."
  (setq hq-inventory (delete item-id hq-inventory))
  (hq-save-data))

;; Основной интерфейс магазина
(defun hq-market ()
  "Открыть интерактивный магазин с кнопками покупки и использования."
  (interactive)
  (hq-market-check-discount)
  (let ((buffer (get-buffer-create "*Habit Market*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Заголовок и баланс
        (insert 
         (propertize "🏪 HABIT MARKET\n" 
                     'face '(:height 1.5 :weight bold :foreground "#4A90E2"))
         (propertize "============================\n"
                     'face '(:foreground "#4A90E2"))
         (propertize (format "💰 Баланс: %d золота\n" hq-gold)
                     'face '(:weight bold)))
        
        ;; Информация о скидке
        (when hq-market-discount
          (insert 
           (propertize (format "🔥 СКИДКА %d%%! 🔥\n" hq-market-discount)
                       'face '(:foreground "#FF5722" :weight bold))))
        
        (insert "\n")
        
        ;; Отображение товаров по категориям
        (dolist (category hq-market-categories)
          (let ((category-id (plist-get category :id))
                (category-name (plist-get category :name))
                (category-icon (plist-get category :icon)))
            
            ;; Заголовок категории
            (insert 
             (propertize (format "%s %s\n" category-icon category-name)
                         'face '(:weight bold :height 1.2 :foreground "#2196F3")))
            
            ;; Товары категории
            (dolist (item hq-market-items)
              (when (string= (plist-get item :category) category-id)
                (let* ((item-id (plist-get item :id))
                       (item-name (plist-get item :name))
                       (item-desc (plist-get item :description))
                       (item-cost (plist-get item :cost))
                       (discountable (not (eq (plist-get item :discountable) nil)))
                       (final-cost (if (and hq-market-discount discountable)
                                     (round (* item-cost (- 1 (/ hq-market-discount 100.0))))
                                   item-cost))
                       (in-inventory (member item-id hq-inventory)))
                  
                  ;; Название, цена и кнопка в одной строке
                  (insert "  ")
                  (insert (propertize item-name 'face '(:weight bold)))
                  (insert " - ")
                  
                  ;; Цена
                  (if (and hq-market-discount discountable)
                      (insert (propertize (format "🪙%d→%d" item-cost final-cost)
                                        'face '(:foreground "#FFD700" :weight bold)))
                    (insert (propertize (format "🪙%d" final-cost)
                                      'face '(:foreground "#FFD700" :weight bold))))
                  
                  ;; Кнопка действия
                  (insert " ")
                  (if in-inventory
                      (insert-text-button "Использовать"
                                        'action (lambda (_) (hq-market-use-item item-id))
                                        'follow-link t
                                        'face '(:foreground "white" :background "#4CAF50" :weight bold))
                    (if (>= hq-gold final-cost)
                        (insert-text-button "Купить"
                                          'action (lambda (_) (hq-market-buy-item item-id))
                                          'follow-link t
                                          'face '(:foreground "white" :background "#2196F3" :weight bold))
                      (insert (propertize " ❌ Недостаточно золота"
                                        'face '(:foreground "#FF5722")))))
                  
                  ;; Описание товара на следующей строке мелким шрифтом
                  (insert "\n    ")
                  (insert (propertize item-desc
                                    'face '(:slant italic :height 0.9 :foreground "#666666")))
                  (insert "\n"))))
            (insert "\n")))
        
        ;; Инвентарь
        (insert 
         (propertize "📦 Ваш инвентарь\n"
                     'face '(:weight bold :height 1.1)))
        (if hq-inventory
            (dolist (item-id hq-inventory)
              (let* ((item (seq-find (lambda (i) 
                                      (string= (plist-get i :id) item-id))
                                    hq-market-items))
                     (item-name (plist-get item :name)))
                (insert "  • " item-name "\n")))
          (insert (propertize "  Инвентарь пуст\n" 
                             'face '(:foreground "#888888" :slant italic)))))
      
      (special-mode)
      (local-set-key "q" 'quit-window)
      (local-set-key "r" 'hq-market))
    
    (switch-to-buffer buffer)))

(defun hq-market-buy-item (item-id)
  "Купить предмет из магазина."
  (let* ((item (seq-find (lambda (i) 
                          (string= (plist-get i :id) item-id))
                        hq-market-items))
         (cost (plist-get item :cost))
         (discountable (not (eq (plist-get item :discountable) nil)))
         (final-cost (if (and hq-market-discount discountable)
                        (round (* cost (- 1 (/ hq-market-discount 100.0))))
                      cost)))
    (if (>= hq-gold final-cost)
        (progn
          (setq hq-gold (- hq-gold final-cost))
          (hq-market-add-to-inventory item-id)
          (message "✨ Вы купили %s за %d золота!" 
                   (plist-get item :name) final-cost))
      (message "❌ Недостаточно золота для покупки %s!" 
               (plist-get item :name)))
    (hq-market)))

(defun hq-market-use-item (item-id)
  "Использовать предмет из инвентаря."
  (let* ((item (seq-find (lambda (i) 
                          (string= (plist-get i :id) item-id))
                        hq-market-items))
         (use-message (plist-get item :use-message)))
    (hq-market-remove-from-inventory item-id)
    (message "🎉 %s" use-message)
    (hq-market)))

;; Глобальная привязка клавиш для магазина
(global-set-key (kbd "C-c m") 'hq-market)

(provide 'market)
