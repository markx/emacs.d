
(use-package gruvbox-theme)
(use-package darktooth-theme)
;;(use-package monokai-theme)
(use-package atom-one-dark-theme)
(use-package zenburn-theme)

(if (display-graphic-p)
  (load-theme 'atom-one-dark)
  (load-theme 'zenburn))

(defun switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'init-theme)
