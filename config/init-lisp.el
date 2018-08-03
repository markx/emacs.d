
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((lisp-mode
          emacs-lisp-mode
          scheme-mode) .
         rainbow-delimiters-mode))


(use-package slime
  :ensure t
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q run")
  (setq slime-contribs '(slime-fancy)))


(use-package geiser
  :config
  (setq geiser-active-implementations '(chicken guile racket)))


(use-package racket-mode
  :config
  (define-key racket-repl-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
  (define-key racket-repl-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
  )

(use-package lispyville)


(defun my/parinfer-enable ()
  (parinfer-mode)
  (electric-pair-local-mode 0) ; parinfer will insert close-paren
  )

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :hook ((emacs-lisp-mode
          scheme-mode
          racket-mode
          lisp-mode) . #'my/parinfer-enable)
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    ))


(provide 'init-lisp)
