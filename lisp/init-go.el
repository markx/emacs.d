(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'go-mode-setup))

(defun go-mode-setup ()
  ;;(go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))


(provide 'init-go)
