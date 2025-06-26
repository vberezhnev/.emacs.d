(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c g" . org-id-get-create)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n b" . orb-insert-link)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-db-autosync-mode t)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("a" "Atomic note (with source)" plain (file "~/Org/Templates/Atomic note.org")
      :if-new
      (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org" "#+startup: latexpreview\n#+date: %U\n#+title: ${title}\n")
      :unnarrowed t)
     
     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
      :unnarrowed t)
     
     ("r" "Bibliography reference" plain (file "~/Org/Templates/Bibliography reference.org") ; <-- template store in a separate file
      :target
      (file+head "bibliography/references/${citekey}.org" "#+title: ${title}\n#+date: %U")
      :unnarrowed t)))
  
  (org-roam-capture-ref-templates
   '(("r" "ref" plain
      "%?"
      :target (file+head "web/${slug}.org"
                         "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n%(zp/org-protocol-insert-selection-dwim \"%i\")")
      :unnarrowed t)
     ("i" "incremental" plain
      "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
      :target (file+head "web/${slug}.org"
                         "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
      :unnarrowed t
      :empty-lines-before 1)))

  (org-roam-dailies-capture-templates
   '(("d" "Дневник продуктивности - утро" plain (file "~/Org/Templates/journal/Morning.org")
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

     ("D" "Дневник продуктивности - вечер" plain (file "~/Org/Templates/journal/Evening.org")
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))

     ("j" "Мысли" plain "* %U"
      :if-new (file+head "%<%Y-%m-%d>.org" "* %U\n#+title: %U\n\n"))))

  
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  ;; Enable org-roam-dailies
  ;; (require 'org-roam-dailies)
  ;; Enable db autosync to ensure node metadata is up-to-date
  

;; (defun my/org-roam-dailies-customize ()
;;   "Применяет кастомизации только для файлов org-roam-dailies."
;;   ;; (when (and buffer-file-name
;;   ;;            (string-prefix-p
;;   ;;             (expand-file-name (concat org-roam-directory "/" org-roam-dailies-directory))
;;   ;;             buffer-file-name)))
;;     ;; Здесь будут настройки для изменения внешнего вида
;;     (my/apply-journal-style))

;; (defun my/apply-journal-style ()
;;   "Применяет уникальный стиль для org-roam-dailies."
;;   ;; Установить другой шрифт
;;   (buffer-face-set '(:family "Iosevka" :height 140 :weight regular))
  
;;   ;; Отключить номера строк и другие отвлекающие элементы
;;   (display-line-numbers-mode -1)
;;   (hl-line-mode -1)
  
;;   ;; Изменить фон и цвета текста для атмосферы
;;   (set-background-color "#1C2526")
;;   (set-foreground-color "#E8E8E8")
  
;;   ;; Убрать панель инструментов и меню
;;   (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;   (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  
;;   ;; Включить режим writeroom для минимализма
;;   (writeroom-mode 1)
  
;;   ;; Опционально: включить эффект печатной машинки
;;   (when (fboundp 'typewriter-mode) (typewriter-mode 1))
  
;;   ;; Скрыть заголовки и свойства Org-mode для чистоты
;;   (org-appear-mode 1)
;;   (setq-local org-hide-emphasis-markers t)
;;   (setq-local org-pretty-entities t))

;; (defun my/reset-journal-style ()
;;   "Сбрасывает кастомизации при выходе из файла журнала."
;;   ;; Восстановить стандартный шрифт
;;   (buffer-face-set nil)
  
;;   ;; Восстановить фон и цвета
;;   (set-background-color nil)
;;   (set-foreground-color nil)
  
;;   ;; Вернуть панели и настройки
;;   (when (fboundp 'tool-bar-mode) (tool-bar-mode 1))
;;   (when (fboundp 'menu-bar-mode) (menu-bar-mode 1))
  
;;   ;; Отключить writeroom и typewriter
;;   (writeroom-mode -1)
;;   (when (fboundp 'typewriter-mode) (typewriter-mode -1))
  
;;   ;; Восстановить номера строк и другие режимы
;;   (display-line-numbers-mode 1)
;;   (hl-line-mode 1))

;; (add-hook 'org-roam-dailies-mode-hook #'my/org-roam-dailies-customize)

;; ;; Применять сброс при закрытии файла журнала
;; (add-hook 'kill-buffer-hook
;;           (lambda ()
;;             (when (and buffer-file-name
;;                        (string-prefix-p
;;                         (expand-file-name (concat org-roam-directory "/" org-roam-dailies-directory))
;;                         buffer-file-name))
;;               (my/reset-journal-style))))

;; (use-package writeroom-mode
;;   :ensure t
;;   :config
;;   (setq writeroom-width 80))

  ;; ;; Fix before-save-hook to avoid org-roam-node nil error
  ;; (defun my/org-roam-safe-save ()
  ;;   "Safely save org-roam-dailies buffer without node errors."
  ;;   (when (and (bound-and-true-p org-roam-dailies-mode)
  ;;              (buffer-file-name)
  ;;              (string-match-p (regexp-quote org-roam-dailies-directory)
  ;;                              (buffer-file-name)))
  ;;     ;; Ensure buffer is recognized as an org-roam node
  ;;     (unless (org-roam-node-at-point)
  ;;       (org-roam-node-insert))
  ;;     ;; Run org-roam save hooks safely
  ;;     (condition-case err
  ;;         (org-roam-db-sync)
  ;;       (error (message "Org-roam db sync error: %s" err)))))
  
  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
        '("title" "citekey"  "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  ;; (setq org-roam-database-connector 'sqlite)
  (org-roam-db-autosync-mode)
  (setq fill-prefix "")
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:60}" 'face 'org-tag)))

  ;;for org-roam-buffer-toggle
  ;;Recommendation in the official manual
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*org-roam\\*"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 0.33)
  ;;                (window-height . fit-window-to-buffer)))
  )
  ;; Add hooks
  ;; :hook
  ;; ((org-roam-dailies-mode . my/org-roam-dailies-setup)
  ;;  (kill-buffer-hook . my/org-roam-dailies-cleanup))
  ;;  (before-save-hook . my/org-roam-safe-save))


(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :config (org-roam-timestamps-mode)
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t))

(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

        Normally, insert the headline’s title using the ’#title:’ file-level property
        and delete the Org-mode headline. However, if the current headline has a
        Org-mode properties drawer already, keep the headline and don’t insert
        ‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
        ‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find 'other-window title nil)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(defun org-roam-insert-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

        Normally, insert the headline’s title using the ’#title:’ file-level property
        and delete the Org-mode headline. However, if the current headline has a
        Org-mode properties drawer already, keep the headline and don’t insert
        ‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
        ‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find 'other-window title nil)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

;; (use-package org-roam-ui
;;   :straight
;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

;; (use-package org-readwise
;;   :quelpa (org-readwise :fetcher github :repo "CountGreven/org-readwise")
;;   :config
;;   ;; Ensure auth-source is configured to find your Readwise token
;;   (setq auth-sources '("~/.authinfo"))

;; Set the output location for your highlights (buffer or file)
;; (setq org-readwise-output-location "~/Org/readwise-highlights.org")

;; ;; Optionally set the debug level (0 = no debug, 1 = basic debug, 2 = detailed debug)
;; (setq org-readwise-debug-level 1))

(use-package org-ref
  :quelpa (org-ref
	   :fetcher github
	   :repo "jkitchin/org-ref"
	   :branch "master")
  :config
  (require 'org-ref)
  ;; (require 'org-ref-helm)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  
  (setq bibtex-completion-bibliography '("~/Org/Bibliography/Bibliography.bib")
	bibtex-completion-library-path '("~/Org/Bibliography/files/")
	bibtex-completion-notes-path "~/Org/Bibliography/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath)))
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5))

(use-package org-roam-bibtex
  :after org-roam
  :quelpa (org-roam-bibtex
	   :fetcher github
	   :repo "org-roam/org-roam-bibtex"
	   :branch "main"))
:config
(require 'org-ref) ; optional: if using Org-ref v2 or v3 citation links

(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))

(org-link-set-parameters "zotero" :follow 'my-org-zotero-open)

(use-package citar
  :quelpa (citar
	   :fetcher github
	   :repo "emacs-citar/citar"
	   :branch "main")
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Org/Bibliography/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :config
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
	      "speaker_notes"
	      :face 'all-the-icons-blue
	      :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes")))

;; (use-package doct
;;   :ensure t
;;   :after org-roam
;;   :config
;;   ;; Ensure org-roam and org-roam-dailies are loaded
;;   (require 'org-roam)
;;   (require 'org-roam-dailies)

;;   ;; Set org-roam directories (adjust paths as needed)
;;   (setq org-roam-directory "~/Org/Org-roam"
;;         org-roam-dailies-directory "~/Org/Org-roam/journal")

;;   ;; Define org-roam capture templates
;;   (setq org-roam-capture-templates
;;         (doct
;;          '(("Atomic note (with source)"
;;             :keys "a"
;;             :type plain
;;             :file "~/Org/Templates/Atomic note.org"
;;             :target (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org"
;; 			       "#+startup: latexpreview\n#+date: %U\n#+title: ${title}\n")
;;             :unnarrowed t)
;;            ("Biography (Person)"
;;             :keys "b"
;;             :type plain
;;             :file "~/Org/Templates/Person.org"
;;             :target (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org"
;; 			       "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
;;             :unnarrowed t)
;;            ("Bibliography reference"
;;             :keys "r"
;;             :type plain
;;             :file "~/Org/Templates/Bibliography reference.org"
;;             :target (file+head "bibliography/references/${citekey}.org"
;; 			       "#+title: ${title}\n#+date: %U")
;;             :unnarrowed t))))

;;   ;; Define org-roam capture ref templates
;;   ;; (setq org-roam-capture-ref-templates
;;   ;;       (doct
;;   ;;        '(("Ref"
;;   ;;           :keys "r"
;;   ;;           :type plain
;;   ;;           :template "%?"
;;   ;;           :target (file+head "web/${slug}.org"
;;   ;;                             "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n%(zp/org-protocol-insert-selection-dwim \"%i\")")
;;   ;;           :unnarrowed t)
;;   ;;          ("Incremental"
;;   ;;           :keys "i"
;;   ;;           :type plain
;;   ;;           :template "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
;;   ;;           :target (file+head "web/${slug}.org"
;;   ;;                             "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
;;   ;;           :unnarrowed t
;;   ;;           :empty-lines-before 1))))

;;   ;; Define org-roam dailies capture templates
;;   (setq org-roam-dailies-capture-templates
;;         (doct
;;          '(("Дневник продуктивности - утро"
;;             :keys "d"
;;             :type plain
;;             :file "~/Org/Templates/journal/Morning.org"
;;             :target (file+head "%<%Y-%m-%d>.org"
;; 			       "* %U\n#+title: %U\n\n"))
;;            ("Дневник продуктивности - вечер"
;;             :keys "D"
;;             :type plain
;;             :file "~/Org/Templates/journal/Evening.org"
;;             :target (file+head "%<%Y-%m-%d>.org"
;; 			       "* %U\n#+title: %U\n\n"))
;;            ;; ("Мысли"
;;            ;;  :keys "j"
;;            ;;  :type plain
;;            ;;  :template "* %U"
;;            ;;  :target (file+head "%<%Y-%m-%d>.org"
;;            ;;                     "* %U\n#+title: %U\n\n"))
;; 	   )))

;;   ;; Enable epa file caching for symmetric encryption
;;   (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;; (use-package khoj
;;   :ensure t
;;   :pin melpa-stable
;;   :bind ("C-c s" . 'khoj)
;;   :config (setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY"
;;                 khoj-server-url "https://app.khoj.dev"))
