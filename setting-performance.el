(setq gc-cons-threshold most-positive-fixnum) ; Minimize garbage collection during startup

(add-hook 'emacs-startup-hook ; Lower threshold back to 8 MiB (default is 800kB)
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))
