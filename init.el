(require 'package)
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
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
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(unless (display-graphic-p)
    (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'linum-mode)
(global-hl-line-mode)
(setq linum-format "%4d  ")
(add-hook 'prog-mode-hook 'electric-pair-mode)
;;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;;(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
;;(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
;;(setq large-file-warning-threshold nil)
;;(setq split-width-threshold nil)
(setq custom-safe-themes t)
;;(put 'narrow-to-region 'disabled nil)
(set-frame-font "Source Code Pro for Powerline-12" nil t)
(setq create-lockfiles nil)
(setq backup-by-copying t) ;;stop emacs's backup changing the file's creation date of the original file


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
(require 'init-helm)
(require 'init-company)
(require 'init-go)
(require 'init-python)
(require 'init-racket)
(require 'init-theme)

(use-package projectile
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :defer t)

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
   (let* ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "node_modules"))
          (eslint (and root
                       (expand-file-name "node_modules/eslint/bin/eslint.js"
                                         root))))
     (when (and eslint (file-executable-p eslint))
       (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (add-hook 'flycheck-mode-hook
    (lambda ()
      (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
      (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
      (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error))))

(use-package which-key
  :diminish ""
  :config
  (which-key-mode t))

(use-package neotree
  :config
  (setq neo-theme 'arrow)
  (global-set-key [f7] 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))


(use-package indent-guide
  :diminish ""
  :config
  (setq indent-guide-recursive t)
  (indent-guide-global-mode))

(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))

(use-package ag)

(use-package parinfer
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            ;; paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)
    (add-hook 'racket-mode-hook #'parinfer-mode)))

(use-package rainbow-delimiters)

(use-package smart-mode-line
  :config
  (smart-mode-line-enable))

(provide 'init)
;;; init.el ends here
