;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;; ;; ORG ROAM SETTING ;; ;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;; ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c g" . org-id-get-create)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :custom
  (org-roam-directory (file-truename "~/Org/Org-roam"))
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("a" "Atomic note (with source)" plain (file "~/Org/Templates/Atomic note.org")
      :if-new
      (file+head "%<%Y-%m-%d-%H:%M>--${slug}.org" "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("t" "Article" plain (file "~/Org/Templates/Article.org")
      :if-new
      (file+head "articles/%<%Y-%m-%d-%H:%M>-article--${slug}.org" "#+title: ${title}\n#+filetags: :Article:\n#+date: %U\n\n")
      :unnarrowed t)
     ("t" "Thought" plain "%?"
      :if-new (file+head "thoughts/%<%Y-%m-%d-%H:%M>--thought-${slug}.org" "#+title: ${title}\n#+filetags: :Thought:\n#+date: %U\n\n\n* See also:\n+ ")
      :unnarrowed t)
     ("b" "Biography (Person)" plain (file "~/Org/Templates/Person.org")
      :if-new (file+head "persons/%<%Y-%m-%d-%H:%M>--person-${slug}.org" "#+title: ${title}\n#+filetags: :Biography:\n#+date: %U\n")
      :unnarrowed t)
     ("p" "Project" plain (file "~/Org/Templates/Project.org")
      :if-new (file+head "projects/%<%Y-%m-%d-%H:%M>--project-${slug}.org" "#+title: ${title}\n#+filetags: :Project:\n#+date: %U\n\n")
      :unnarrowed t)
     ("b" "Book notes" plain (file "~/Org/Templates/Book.org")
      :if-new (file+head "%<%Y-%m-%d-%H:%M>--book-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :Book:%^{Book type}:\n\n")
      :unnarrowed t)
     ("r" "Bibliography reference" plain (file "~/Org/Templates/Bibliography reference.org") ; <-- template store in a separate file
      :target
      (file+head "bibliography/references/${citekey}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :Book:%^{Book type}:")
      :unnarrowed t)))
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (org-roam-dailies-capture-templates
   '(
     ("1" "Morning diary №1" plain (file "~/Org/Templates/journal/Morning.org") :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %U\n\n" ))
     ("2" "Morning diary №2" entry "* Default (%U): «%?»‎\n\n#+begin_quote\n%^{Quote}\n#+end_quote" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %U\n\n" ))
     
     ("d" "Default diary" entry "* Default (%U): «%?»‎\n\n" :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %U\n\n" ))
     
     ("e" "Evening diary" plain (file "~/Org/Templates/journal/Evening.org") :clock-in t :clock-resume t
      :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %U\n\n" ))))
  ;; Org-noter integration with org-roam-bibtex
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-attached-file-extensions '("pdf"))
  :config
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  ;; Customize the org-roam buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(defun org-roam-buffer-setup ()
  "Function to make org-roam-buffer more pretty."
  (progn
    (setq-local olivetti-body-width 44)
    (variable-pitch-mode 1)
    (olivetti-mode 1)
    (centaur-tabs-local-mode -1)

  (set-face-background 'magit-section-highlight (face-background 'default))))

;; (after! org-roam
(add-hook 'org-roam-mode-hook 'org-roam-buffer-setup)

(use-package org-roam-ui
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq orui-sync-theme nil
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil ))

(use-package org-roam-timestamps
  :after org-roam
  :demand t
  :config (org-roam-timestamps-mode))
(setq org-roam-timestamps-parent-file t)
(setq org-roam-timestamps-remember-timestamps t)

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

(provide 'org-roam-setting)
