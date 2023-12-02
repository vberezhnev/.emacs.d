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

(provide 'formatter-setting)
