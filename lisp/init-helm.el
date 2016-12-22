(use-package helm
  :diminish helm-mode
  :commands helm-mode
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action)) ; list actions using C-z

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project))

(provide 'init-helm)
