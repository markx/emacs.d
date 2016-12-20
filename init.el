(require 'package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-linum-mode)
;;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;;(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
;;(eval-after-load "vc" '(setq vc-handled-backends nil))
;;(setq vc-follow-symlinks t)
;;(setq large-file-warning-threshold nil)
;;(setq split-width-threshold nil)
(setq custom-safe-themes t)
;;(put 'narrow-to-region 'disabled nil)
(set-frame-font "Source Code Pro for Powerline-12" nil t)


(defconst emacs-tmp-dir (format "%s%s-%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(require 'init-evil)

(use-package helm
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t))

(use-package projectile
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :commands (helm-projectile helm-projectile-switch-project))

(use-package company
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (use-package company-quickhelp
    :defer 1
    :config
    (company-quickhelp-mode 1)))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :defer t)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook
      (lambda ()
        (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
        (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
        (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))))

(use-package gruvbox-theme)
(use-package darktooth-theme)
(use-package monokai-theme)
(use-package atom-one-dark-theme)
(load-theme 'atom-one-dark)

(use-package which-key
  :diminish ""
  :config
  (which-key-mode t))

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

(use-package indent-guide
  :config
  (setq indent-guide-recursive t)
  (indent-guide-global-mode))

(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))

(provide 'init)
;;; init.el ends here
