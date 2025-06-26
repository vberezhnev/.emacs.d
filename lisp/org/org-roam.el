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

  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))


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

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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

;; (use-package khoj
;;   :ensure t
;;   :pin melpa-stable
;;   :bind ("C-c s" . 'khoj)
;;   :config (setq khoj-api-key "YOUR_KHOJ_CLOUD_API_KEY"
;;                 khoj-server-url "https://app.khoj.dev"))
