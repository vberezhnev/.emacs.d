;;________________________________________________________________
;;    Telega.el
;;________________________________________________________________
(use-package telega
  :config
  (setq telega-use-docker t)
  (add-hook 'telega-load-hook 'telega-notifications-mode)
  (add-hook 'telega-load-hook 'telega-appindicator-mode)
  (add-hook 'telega-load-hook 'global-telega-url-shorten-mode))

;;________________________________________________________________
;;    Matrix
;;________________________________________________________________
(use-package ement)
