;; org-habit-enhanced customizations
(setq hq-quests '((:id 1 :name "Fitness Challenge" :description "Exercise daily for 7 days" :habits ("🏋️ - Workout") :required 7 :progress 0 :completed nil :reward-xp 250 :reward-gold 120) (:id 2 :name "Reading Goal" :description "Read 30 minutes daily for 5 days" :habits ("📚 - Read 30min") :required 5 :progress 0 :completed nil :reward-xp 150 :reward-gold 80)))
(setq hq-task-rewards '(("WORK" :xp 60 :gold 50) ("STUDY" :xp 45 :gold 35) ("PERSONAL" :xp 20 :gold 15)))
(setq hq-category-colors '(("WORK" . "#FF6347") ("STUDY" . "#4682B4") ("PERSONAL" . "#32CD32")))
(setq hq-market-items '((:id "coffee" :name "Coffee Break" :cost 20 :category "rest" :description "Enjoy a quick coffee break" :use-message "Refreshing coffee break!" :discountable t) (:id "book" :name "New Book" :cost 200 :category "rare" :description "Buy a new book to read" :use-message "Happy reading!" :discountable nil)))
