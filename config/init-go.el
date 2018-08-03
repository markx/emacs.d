
(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook 'go-mode-setup))


(use-package lsp-go
  :ensure t
  :requires (lsp-mode go-mode)
  :defer t
  :hook (go-mode-hook . lsp-go-enable))

(defun go-mode-setup ()
  ;;(go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))


(provide 'init-go)
