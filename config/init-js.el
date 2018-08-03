
(use-package js2-mode
  ;; :mode "\\.js\\'"
  :config
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-mode-show-parse-errors nil))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package lsp-javascript-typescript
  :ensure t
  :requires lsp-mode
  :config
  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode-hook #'lsp-javascript-typescript-enable))


(provide 'init-js)
