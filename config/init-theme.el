
(use-package gruvbox-theme)
(use-package darktooth-theme)
;;(use-package monokai-theme)
(use-package atom-one-dark-theme)
(use-package zenburn-theme)

(if (display-graphic-p)
  (load-theme 'atom-one-dark)
  (load-theme 'zenburn))

(defun switch-theme (theme)
  "Disable any loaded themes before enabling a new theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(setq my-partial-theme-list '(smart-mode-line-dark
                              smart-mode-line-light
                              smart-mode-line-respectful
                              ))

(defadvice load-theme (before dont-propagate-theme (theme &rest args) activate)
  (unless (memq theme my-partial-theme-list)
    (mapc #'disable-theme custom-enabled-themes)))

(provide 'init-theme)
