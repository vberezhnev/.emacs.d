(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") 'ian/format-code)
  (global-set-key (kbd "C-c f") 'format-all-buffer)
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter))

;; (use-package prettier-js
;;   :config

;; (defun enable-minor-mode (my-pair)
;;   "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
;;   (if (buffer-file-name)
;;       (if (string-match (car my-pair) buffer-file-name)
;;           (funcall (cdr my-pair)))))

;; (add-hook 'web-mode-hook #'(lambda ()
;;                              (enable-minor-mode
;;                               '("\\.jsx?\\'" . prettier-js-mode))))
;; (add-hook 'web-mode-hook 'prettier-js-mode))

;;;;;;;;;;;;;;;;

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :config
;;   (apheleia-global-mode +1))

(provide 'formatter-setting)
