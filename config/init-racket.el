
(use-package racket-mode
  :config
  (define-key racket-repl-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)
  (define-key racket-repl-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
  )

(provide 'init-racket)
