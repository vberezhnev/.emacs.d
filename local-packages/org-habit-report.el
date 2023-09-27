(defvar ol/habit-report-defaultday 30
  "The default range of days from today, when no time is specified.")

(defun ol/get-org-habit-string (&optional block starttime endtime)
  ;; check if starttime and endtime is specified
  (or starttime (setq starttime (format-time-string "%a %b %e %H:%M:%S %G" (time-subtract (current-time) (days-to-time ol/habit-report-defaultday)))))
  (or endtime (setq endtime (current-time-string)))

  ;; when block is specified set starttime and endtime
  (when block
    (progn
      (setq cc (org-clock-special-range block nil t)
	    starttime (car cc)
	    endtime (nth 1 cc))))

  ;; build the habit graph
  (list (org-habit-build-graph
	 (org-habit-parse-todo)
         ;; time from
	 (org-time-subtract (date-to-time starttime) (* 3600 org-extend-today-until))
         ;; today
	 (date-to-time endtime)
         ;; time to
	 (date-to-time endtime)) starttime endtime))

(defun ol/habit-report (&optional params)
  (save-excursion
    (org-back-to-heading t)
    (print (ol/get-org-habit-string (plist-get params :block) (plist-get params :tstart) (plist-get params :tend)))
    (let* ((habit-data (ol/get-org-habit-string (plist-get params :block) (plist-get params :tstart) (plist-get params :tend)))
           (habit-str (car habit-data))
           (face-counts (list (cons 'org-habit-clear-future-face  0)
                              (cons 'org-habit-ready-face  0)
                              (cons 'org-habit-ready-future-face  0)
                              (cons 'org-habit-alert-future-face  0)
                              (cons 'org-habit-overdue-face  0)))
           (habit-stats (list (cons :org-heading  (org-get-heading t t t t))
                              (cons :habit-done  0)
                              (cons :habit-missed  0)
                              (cons :habit-last-missed  nil)
                              (cons :longest-day-streak  0)
                              (cons :longest-done-streak  0)
                              (cons :current-longest-done-streak  nil)
                              (cons :starttime (car (cdr habit-data)))
                              (cons :endtime (car (cdr(cdr habit-data))))))
           (cur-day-streak 0)
           (cur-done-streak 0))

      ;; iterate over string
      (dotimes (i (length habit-str))

        ;; sum up all faces
        (when (alist-get (get-text-property i 'face habit-str) face-counts)
          (setf (alist-get (get-text-property i 'face habit-str) face-counts) (+ (alist-get (get-text-property i 'face habit-str) face-counts) 1)))

        ;; if face is overdue of alert and has no complete-glyp
        (if (and (or (eq (get-text-property i 'face habit-str)
                         'org-habit-overdue-face)
                     (eq (get-text-property i 'face habit-str)
                         'org-habit-alert-future-face))
                 (not
                  (string= (string (aref habit-str i))
                           (string org-habit-completed-glyph))))

            (progn
              (setf (alist-get :habit-last-missed habit-stats) (get-text-property i 'help-echo habit-str))
              (when (> cur-day-streak (alist-get :longest-day-streak habit-stats))
                (setf (alist-get :longest-day-streak habit-stats) cur-day-streak)
                (setq cur-day-streak 0))
              (when (> cur-done-streak (alist-get :longest-done-streak habit-stats))
                (setf (alist-get :longest-done-streak habit-stats) cur-done-streak)
                (setq cur-done-streak 0)))
          (progn
            (setf cur-day-streak (+ 1 cur-day-streak))
            (when (eq (get-text-property i 'face habit-str)
                      'org-habit-ready-face)
              (setf cur-done-streak (+ 1 cur-done-streak))))
          )
        (if (string= (string (aref habit-str i))
                     (string org-habit-completed-glyph))
            (setf (alist-get :habit-done habit-stats) (+ 1 (alist-get :habit-done habit-stats))))
        ) ;; string iteration done

      ;; when last streak bigger then last streak
      (when (> cur-day-streak (alist-get :longest-day-streak habit-stats))
        (setf (alist-get :longest-day-streak habit-stats) cur-day-streak))
      (when (> cur-done-streak (alist-get :longest-done-streak habit-stats))
        (setf (alist-get :longest-done-streak habit-stats) cur-done-streak)
        (setf (alist-get :current-longest-done-streak habit-stats) t))

      ;; set missed habit count
      (setf (alist-get :habit-missed habit-stats) (alist-get 'org-habit-overdue-face face-counts))

      habit-stats)))

(defun ol/habit-print-header (st et)
  (format "#+CAPTION: Habit report from %s to %s
| Heading | Done Count | Missed Count | Last Missed | Longest Streak (days) | Longest Streak (done) | Currently longest |
|-- |" st et))

(defun ol/habit-stats-to-string (org-habits)
  (concat (ol/habit-print-header
           (format-time-string "%d-%m-%y" (date-to-time (alist-get :starttime (car org-habits))))
           (format-time-string "%d-%m-%y" (date-to-time (alist-get :endtime (car org-habits)))))
          (let ((result ""))(dolist (org-habit org-habits result)
                              (setq result (concat result (format "\n|%s| %S | %s | %s | %s | %s | %s |"
                                                                  (alist-get :org-heading org-habit)
                                                                  (alist-get :habit-done org-habit)
                                                                  (alist-get :habit-missed org-habit)
                                                                  (alist-get :habit-last-missed org-habit)
                                                                  (alist-get :longest-day-streak org-habit)
                                                                  (alist-get :longest-done-streak org-habit)
                                                                  (alist-get :current-longest-done-streak org-habit))))))))

(defun org-dblock-write:ol/habit-report (params)
  (if (plist-get params :scope)
      (setq ol/scope (plist-get params :scope))
    (setq ol/scope 'tree))
  (insert (ol/habit-stats-to-string
           (org-map-entries (lambda () (ol/habit-report params)) "STYLE=\"habit\"" ol/scope)))
  (org-table-align))
