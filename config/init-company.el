(use-package company
  :defer t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (mapc #'evil-declare-change-repeat
        '(company-complete
          company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          ))

  ;; (use-package company-quickhelp
  ;;   :config
  ;;   (setq company-quickhelp-delay 0.5)
  ;;   (company-quickhelp-mode 1))
  
  (use-package company-tern
    :defer 1
    :config
    (add-to-list 'company-backends 'company-tern)))


(provide 'init-company)
