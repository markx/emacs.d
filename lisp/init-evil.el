
(defun evil-paste-from-clipboard ()
  (interactive)
  (evil-paste-from-register ?+))

(defun evil-copy-to-clipboard ()
  (interactive)
  (evil-use-register ?+)
  (evil-yank))

(use-package evil
  :diminish undo-tree-mode 
  :init
  (setq evil-want-C-u-scroll t)
  (setq select-enable-clipboard nil)

  (setq evil-emacs-state-modes nil)
  ;;(setq evil-motion-state-modes nil)


  (use-package evil-leader
    :config
    (global-evil-leader-mode) ;;need to enable global-evil-leader-mode before evil-mode, so leader can work in in initial buffers (*scratch*, *Messages*, …).
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "<SPC>" 'helm-M-x)
    (evil-leader/set-key "r" 'helm-imenu)
    (evil-leader/set-key "b" 'helm-mini);; change to another buffer
    (evil-leader/set-key "pp" 'helm-projectile)
    (evil-leader/set-key "pr" 'projectile-switch-project)
    (evil-leader/set-key "1" 'delete-other-windows)
    (evil-leader/set-key "f" 'helm-find-files)
    (evil-leader/set-key "c SPC" 'comment-line))

  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line) 
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line) 
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile) ;;ctrlp like
  (define-key evil-normal-state-map (kbd "C-w q") 'evil-window-delete)
  (define-key evil-visual-state-map (kbd "C-w q") 'evil-window-delete)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-insert-state-map (kbd "s-c") 'clipboard-kill-region)
  (define-key evil-insert-state-map (kbd "s-v") 'evil-paste-from-clipboard)

  (define-key evil-visual-state-map (kbd "SPC c SPC") 'comment-dwim)

  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "h[elp]" 'help)))



(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "jk")
  (global-set-key [escape] 'evil-escape)
  (evil-escape-mode))


(provide 'init-evil)

