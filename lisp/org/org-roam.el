;;; org-roam.el --- Org-roam configuration for note-taking -*- lexical-binding: t; -*-

;; Org-roam: Load for note-taking commands
(use-package org-roam
  :straight t
  :commands (org-roam-buffer-toggle org-roam-node-find org-roam-node-insert
             orgHX2-roam-capture org-roam-tag-add org-roam-ref-add
             org-id-get-create org-roam-dailies-capture-today orb-insert-link)
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
         ("C-M-i" . completion-at-point))
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-db-autosync-mode t)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("a" "Atomic note (with source)" plain (file "~/Org/Templates/Atomic note.org")
      :if-new
      (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org" "#+startup: latexpreview\n#+date: %U\n#+title: ${title}\n")
      :unnarrowed t)
     
     ("p" "Project" plain (file "~/Org/Templates/Project.org")
      :if-new
      (file+head "projects/%<%Y-%m-%d-%H:%M>--${slug}.org" "#+startup: latexpreview\n#+date: %U\n#+title: ${title}\n")
      :unnarrowed t)
     
     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
      :unnarrowed t)
     
     ("c" "Contact" plain (file "~/Org/Templates/Contact.org")
      :if-new (file+head "contacts/%<%Y-%m-%d-%H:%M>--contact-${slug}.org" "#+title: ${title}\n#+filetags: :Contact:\n#+date: %U\n")
      :unnarrowed t)

     ("r" "Bibliography reference" plain (file "~/Org/Templates/Bibliography reference.org")
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
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  (setq orb-preformat-keywords
        '("title" "citekey" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  (setq fill-prefix "")
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:60}" 'face 'org-tag)))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

;; Org-roam-timestamps: Load after org-roam
(use-package org-roam-timestamps
  :straight t
  :after org-roam
  :commands (org-roam-timestamps-mode)
  :config
  (org-roam-timestamps-mode)
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t))

;; Custom functions (unchanged)
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

;; Org-roam-ui: Load after org-roam
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Org-ref: Load for bibliography management
(use-package org-ref
  :straight (:host github :repo "jkitchin/org-ref" :branch "master")
  :after org
  :commands (org-ref-insert-link)
  :bind (:map org-mode-map
         ("C-c ]" . org-ref-insert-link))
  :config
  (require 'org-ref)
  (setq bibtex-completion-bibliography '("~/Org/Bibliography.bib")
        ;; bibtex-completion-library-path '("~/Org/Bibliography/files/")
        ;; bibtex-completion-notes-path "~/Org/Bibliography/notes/"
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

;; Org-roam-bibtex: Load after org-roam
(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex" :branch "main")
  :after org-roam
  :commands (orb-insert-link)
  :config
  (require 'org-ref))

;; Zotero link handler (unchanged)
(defun my-org-zotero-open (path _)
  (call-process "xdg-open" nil nil nil (concat "zotero:" path)))

(org-link-set-parameters "zotero" :follow 'my-org-zotero-open)

;; Citar: Load for citation management
(use-package citar
  :straight (:host github :repo "emacs-citar/citar" :branch "main")
  :after org
  :commands (org-cite-insert citar-open)
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Org/Bibliography/Bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind (:map org-mode-map :package org
         ("C-c b" . org-cite-insert))
  ;; :config
  ;; (defvar citar-indicator-notes-icons
  ;;   (citar-indicator-create
  ;;    :symbol (all-the-icons-material
  ;;             "speaker_notes"
  ;;             :face 'all-the-icons-blue
  ;;             :v-adjust -0.3)
  ;;    :function #'citar-has-notes
  ;;    :padding "  "
  ;;    :tag "has:notes"))
  )

(defun my/org-contacts-files-from-roam ()
  "Возвращает список файлов org-roam с тегом :Contact:."
  (mapcar #'org-roam-node-file
          (seq-filter
           (lambda (node)
             (member "Contact" (org-roam-node-tags node)))
           (org-roam-node-list))))

;; Установка org-contacts-files
(setq org-contacts-files (my/org-contacts-files-from-roam))

;; Обновление org-contacts-files при синхронизации базы org-roam
(add-hook 'org-roam-db-autosync-hook
          (lambda ()
            (setq org-contacts-files (my/org-contacts-files-from-roam))))

;; Тестирование
;; (message "%s" (my/org-contacts-files-from-roam))

(provide 'org-roam)
;;; org-roam.el ends here
