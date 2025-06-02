;;; veg-base.el --- the heart of my personal GNU Emacs configuration.

;;; Code:

;;; Initial setup
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Rebind command to Meta, and option to Super
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

;; Do not show confusing warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Package loading
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Ensure that environment variables within Emacs look the same as in my shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package calendar-norway
  :ensure t
  :config
  ;; tell org to display events from calendar diary
  (setq org-agenda-include-diary t)
  (setq calendar-holidays
        (append
         calendar-norway-raude-dagar
         calendar-norway-andre-merkedagar
         calendar-norway-dst)))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-global-modes '(not org-mode)) ; not in org
  (setq company-dabbrev-other-buffers t)) ; if t, search buffers with the same major mode

(use-package crux
  :ensure t
  :config
  (global-set-key (kbd "C-k") 'crux-smart-kill-line)
  (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c f") 'crux-recentf-find-file)
  (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
  (global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
  ;; (global-set-key (kbd "C-c t") 'crux-visit-shell-buffer)
  )
  
(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (concat user-emacs-directory "resources/dashboard-banner.txt"))
  (setq dashboard-center-content t)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-init-info
                                    dashboard-insert-newline
                                    dashboard-insert-items))
  (setq dashboard-items '((recents . 5))))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq dired-dwim-target t))

(use-package drag-stuff
  :ensure t
  :hook
  ;; We do NOT want these bindings in shell-mode
  (prog-mode . drag-stuff-mode)
  (org-mode . drag-stuff-mode)
  :config
  (define-key drag-stuff-mode-map (kbd "M-p") 'drag-stuff-up)
  (define-key drag-stuff-mode-map (kbd "M-n") 'drag-stuff-down))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package eglot
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))


(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; TODO add indent-bars
(use-package org-download
  :ensure t
  :defer t
  :bind (:map org-mode-map
              ("C-c v" . org-download-clipboard))
  :init
    ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable))
  (setq-default org-download-image-dir "~/org/org-download-images")
  (setq-default org-download-abbreviate-filename-function 'expand-file-name))

(use-package olivetti
  :ensure t
  :hook
  (org-mode . olivetti-mode)
  (Info-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 90)
  (setq olivetti-style t))

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
  :bind (("C-c s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done) 
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill)
         ("C-TAB" . ivy-done)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-reverse-i-search-kill)
	 :map swiper-map
	 ("C-l"   .  nil)
	 ("C-c l" .  swiper-recenter-top-bottom))
  :config
  (ivy-mode 1))

(use-package magit
  :ensure t)

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package mood-line
  :ensure t
  :config (mood-line-mode))

(use-package paren-face
  :ensure t
  :hook
  (emacs-lisp-mode . paren-face-mode))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-restore-mode))

(use-package rainbow-mode
  :ensure t)

(use-package vterm
  :ensure t)

(use-package vundo
  :ensure t
  :config
  (global-set-key (kbd "C-x u") 'vundo))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.7))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(pdf-tools-install)
(setq-default pdf-view-display-size 'fit-page)
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-c m") 'pdf-view-midnight-minor-mode)
  ;; Fix outline view breaking in full screen mode on MacOS
  (setopt pdf-annot-tweak-tooltips nil))

;;; Programming languages
;; TODO elpy
;; TODO eglot, clang
;; TODO x86 lookup
(setq asm-comment-char 35)

;;; Sane defaults
;; Directory for autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq create-lockfiles nil)

;; Start Emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default truncate-lines t)
(setq line-spacing 0.1)

;; No tab indentation
(setq-default indent-tabs-mode nil)

;; Do not reset point when scrolling past end of buffer
(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Making Emacs accept sentences ending in a single space
(setq sentence-end-double-space nil)

(setq c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (c-mode . "k&r")
                       (other . "gnu")))

;; Excluding agenda from recentf
(with-eval-after-load 'vegalv-org
  (setq recentf-exclude org-agenda-files))

(setq kill-whole-line t)
(setq next-line-add-newlines nil)

(blink-cursor-mode 0)
(electric-pair-mode 1)
;; Turning off autocomplete for < in Org-mode (used as snippet marker)
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(provide 'veg-base)
;;; veg-base.el ends here
