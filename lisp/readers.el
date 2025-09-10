;; (use-package image-roll
;;   :straight (:host github :repo "dalanicolai/image-roll.el" :branch "main"))

;; (use-package pdf-continuous-scroll-mode.el
;;   :straight (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el" :branch "master"))

;; Disable large file warning
(setq large-file-warning-threshold nil)

(use-package org-noter
  :straight t)

(use-package justify-kp
  :straight (:host github :repo "Fuco1/justify-kp" :branch "master"))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)  ; Automatically use nov-mode for .epub files
  :config

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.5))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)

  (add-hook 'nov-mode-hook #'(lambda () (setq blink-cursor-mode nil)))

  ;; (require 'justify-kp)
  (setq nov-text-width t)

  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
		 'my-nov-window-configuration-change-hook
		 t))

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
	(let ((max-width (pj-line-width))
	      buffer-read-only)
	  (save-excursion
	    (goto-char (point-min))
	    (while (not (eobp))
	      (when (not (looking-at "^[[:space:]]*$"))
		(goto-char (line-end-position))
		(when (> (shr-pixel-column) max-width)
		  (goto-char (line-beginning-position))
		  (pj-justify)))
	      (forward-line 1))))
      (add-hook 'window-configuration-change-hook
		'my-nov-window-configuration-change-hook
		nil t)))

  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)

  :init
  (require 'nov))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)  ; Automatically use pdf-view-mode for .pdf files
  :config
  (pdf-tools-install :no-query)  ; Install and initialize pdf-tools
  (setq pdf-view-use-scaling t)  ; Enable scaling for better rendering
  (add-hook 'pdf-view-mode-hook #'pdf-annot-minor-mode))  ; Enable annotations

(use-package org-remark
  :straight t
  :after (nov pdf-tools)  ; Ensure nov and pdf-tools are loaded first
  :hook ((nov-mode . org-remark-mode)  ; Enable org-remark for nov-mode
	 (nov-mode . org-remark-nov-mode)
	 (pdf-view-mode . org-remark-mode)  ; Enable org-remark for pdf-view-mode
         (eww-mode . org-remark-mode)
	 (eww-mode . org-remark-eww-mode))  ; Enable org-remark for eww-mode
  :bind (("C-c n m" . org-remark-mark)  ; Mark a region for annotation
         ("C-c n l" . org-remark-mark-line)  ; Mark a line for annotation
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)  ; Open remark at point
         ("C-c n ]" . org-remark-view-next)  ; View next remark
         ("C-c n [" . org-remark-view-prev)  ; View previous remark
         ("C-c n r" . org-remark-remove)  ; Remove remark at point
         ("C-c n d" . org-remark-delete))  ; Delete remark at point
  :config
  ;; (setq org-remark-default-features "~/org/remarks.org")  ; Set file for storing remarks
  ;; (org-remark-global-tracking-mode +1)
  )  ; Enable global tracking for remarks


(use-package calibredb
  :straight t
  :after (nov pdf-tools)  ; Ensure nov and pdf-tools are loaded for ebook viewing
  :init
  (require 'calibredb)
  :config
  ;; Set the path to the Calibre library and database
  (setq calibredb-root-dir "/home/berezhnev/Documents/Calibre/")  ; Adjust to your Calibre library path
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  ;; Path to the calibredb executable
  (setq calibredb-program "/usr/bin/calibredb")  ; Default path on Ubuntu
  ;; SQLite program for Emacs < 29 or without built-in SQLite support
  (setq sql-sqlite-program "/usr/bin/sqlite3")  ; Default SQLite path on Ubuntu
  ;; Virtual libraries for quick filtering
  ;; (setq calibredb-virtual-library-alist
  ;;       '(("1. Fiction" . "fiction epub")
  ;;         ("2. Non-Fiction" . "nonfiction pdf")
  ;;         ("3. To Read" . "toread")))
  ;; Font for calibredb buffers (using Liberation Serif, available in Ubuntu)
  (add-hook 'calibredb-search-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Liberation Serif" :size 14)))
  ;; Enable icons (optional, requires nerd-icons or all-the-icons)
  (setq calibredb-format-icons t)  ; Disable if performance is slow on large libraries

  (setq calibredb-open-epub-with 'nov-mode)  ; Open EPUBs with nov-mode
  (setq calibredb-open-pdf-with 'pdf-view-mode)  ; Open PDFs with pdf-tools
  :bind
  (("C-c c" . calibredb)  ; Open calibredb dashboard
   :map calibredb-search-mode-map
   ("r" . calibredb-search-refresh)  ; Refresh library
   ("RET" . calibredb-find-file)))  ; Open selected book

(defun nov-search (pattern)
  (interactive "sEnter search pattern: ")
  (let ((version nov-epub-version)
        (index 1)
        results)
    (while (< index (1- (length nov-documents)))
      (seq-let (id &rest path) (aref nov-documents index)
        (let (;; HACK: this should be looked up in the manifest
              (imagep (seq-find (lambda (item) (string-match-p (car item) path))
                                image-type-file-name-regexps))
              ;; NOTE: allows resolving image references correctly
              (default-directory (file-name-directory path)))
          (unless imagep
            (with-temp-buffer
              (if (and (version< version "3.0") (eq id nov-toc-id))
                  (insert (nov-ncx-to-html path))
                (insert (nov-slurp path)))
              (goto-char (point-min))
              (when (search-forward pattern nil t)
                (nov-render-html)
                (goto-char (point-min))
                (while (search-forward pattern nil t)
                  (push (list (format "%d %s" index
                                      (replace-regexp-in-string "\n" " "
                                                                (thing-at-point 'line)))
                              index (point))
                        results)))))
          (setq index (1+ index)))))
    ;; (print results)))
    (seq-let (index point) (alist-get (completing-read "Jump to: " (reverse results)) results
                                      nil nil #'string=)
      (nov-goto-document index)
      (goto-char point))))
