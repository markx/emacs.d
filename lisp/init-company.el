(use-package company
  :defer t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (use-package company-quickhelp
    :defer 1
    :config
    (company-quickhelp-mode 1))
  (use-package company-tern
    :defer 1
    :config
    (add-to-list 'company-backends 'company-tern)))


(provide 'init-company)
