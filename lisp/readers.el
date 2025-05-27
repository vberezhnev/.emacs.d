;; https://github.com/chenyanming/calibredb.el
(use-package calibredb
	:ensure t
	:config
	(setq calibredb-root-dir "~/Documents/Calibre Library/")
	(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
	(setq calibredb-format-nerd-icons t)

	;; Org-ref support
	(require 'org-ref)
	(setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
	;; (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
	(setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename))

;; https://github.com/Zweihander-Main/kindle-highlights-to-org/
(use-package kindle-highlights-to-org
  :straight (:host github
									 :repo "Zweihander-Main/kindle-highlights-to-org"
									 :branch "master")
  :after org-roam
  :defer t)

(provide 'readers)
;;; readers.el ends here
