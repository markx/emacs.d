
(setq python-shell-interpreter "python3")

(use-package lsp-python
  :ensure t
  :requires lsp-mode
  :defer t
  :hook (python-mode-hook . lsp-python-enable))



(provide 'init-python)
