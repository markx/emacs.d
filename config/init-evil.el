(require 'simpleclip)
(require 'helm)

(defun my/evil-config ()


  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "C-w q") 'evil-window-delete)
  (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile) ;;ctrlp like

  (define-key evil-insert-state-map (kbd "s-c") 'simpleclip-copy)
  (define-key evil-insert-state-map (kbd "s-v") 'simpleclip-paste)

  (define-key evil-visual-state-map (kbd "SPC c SPC") 'comment-dwim)

  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-/") 'company-complete)
  (evil-define-key 'insert' company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (evil-define-key 'insert' company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)

  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-w") 'evil-delete-backward-word)

  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "h[elp]" 'help))

  (eval-after-load 'helm
    '(progn
      (define-key helm-map (kbd "C-w")  'evil-delete-backward-word)
      (define-key helm-map (kbd "s-v") 'simpleclip-paste)))

  (use-package evil-leader
    :config
    (global-evil-leader-mode) ;;need to enable global-evil-leader-mode before evil-mode, so leader can work in in initial buffers (*scratch*, *Messages*, …).
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "<SPC>" 'helm-M-x)
    (evil-leader/set-key "r" 'helm-imenu)
    (evil-leader/set-key "b" 'helm-mini);; change to another buffer
    (evil-leader/set-key "pp" 'helm-projectile-switch-project)
    (evil-leader/set-key "pr" 'projectile-switch-project)
    (evil-leader/set-key "1" 'delete-other-windows)
    (evil-leader/set-key "f" 'helm-find-files)
    (evil-leader/set-key "c SPC" 'comment-line)))


(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-want-C-u-scroll t)
  (setq select-enable-clipboard nil)
  ;;(setq evil-emacs-state-modes nil)
  ;;(setq evil-motion-state-modes nil)
  :config
  (evil-set-initial-state 'repl-mode 'emacs)
  (my/evil-config)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  (global-set-key [escape] 'evil-escape)
  (evil-escape-mode))


(provide 'init-evil)
