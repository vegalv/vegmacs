;;; Initial setup
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)


;;; Sane defaults
(setq ring-bell-function 'ignore)
(electric-pair-mode 1)


;; Do not show confusing warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))


;;; Package loading
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (dired-mode . hl-line-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

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




(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(defun vegalv/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'.

Source: https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/"
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'vegalv/keyboard-quit-dwim)


(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'veg-base)
