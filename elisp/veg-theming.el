;;; veg-theming.el --- my personal theme configuration using the Modus themes.

;;; Code:

(use-package ef-themes
  :ensure t
  :bind
  ("C-c m" . 'ef-themes-toggle)
  :hook
  (ef-themes-post-load . my-ef-themes-custom-faces)
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-dream ef-reverie))

  (let ((mono-spaced-font "Aporetic Sans Mono")
	(proportionally-spaced-font "Aporetic Sans"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 160)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 160)
    (set-face-attribute 'variable-pitch nil :family proportionally-spaced-font :height 1.0)

    (defun my-ef-themes-custom-faces ()
      "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
      (ef-themes-with-colors
       (custom-set-faces
        ;;; core
        `(fringe ((,c :background ,bg-main)))
        `(line-number ((,c :background ,bg-main)))
        ;;; dashboard
        `(dashboard-text-banner ((,c :weight bold :foreground ,magenta-faint)))
        `(dashboard-heading ((,c :weight bold :foreground ,magenta-faint)))
        ;;; dired
        `(dired-directory ((,c :weight bold)))
        ;;; ivy
        `(ivy-current-match ((,c :inherit swiper-line-face)))
        `(ivy-minibuffer-match-face-1 ((,c :inherit swiper-match-face-1 )))
        `(ivy-minibuffer-match-face-2 ((,c :inherit swiper-match-face-2)))
        `(ivy-minibuffer-match-face-3 ((,c :inherit swiper-match-face-3)))
        `(ivy-minibuffer-match-face-4 ((,c :inherit swiper-match-face-4)))
        `(ivy-org ((,c :inherit org-level-1 :weight normal)))
        ;; `(ivy-subdir ((,c  :weight bold)))
        ;;; mood-line
        `(mood-line-buffer-status-modified ((,c :foreground ,fg-alt)))
        ;; olivetti-mode
        `(olivetti-fringe ((,c :background ,bg-main)))
        ;;; org
        `(org-block ((,c :background ,bg-dim)))
        `(org-block-begin-line ((,c :background ,bg-main)))
        `(org-block-end-line ((,c :background ,bg-main)))
        ;;; swiper
        `(swiper-match-face-1 ((,c :background nil :foreground ,nil)))
        `(swiper-match-face-2 ((,c :weight bold :background nil :foreground ,magenta-warmer)))
        `(swiper-match-face-3 ((,c :weight bold :background nil :foreground ,yellow-cooler)))
        `(swiper-match-face-4 ((,c :weight bold :background nil :foreground ,blue-warmer)))
        `(swiper-line-face ((,c :background ,bg-hl-line :extend t)))
        `(swiper-background-match-face-1 ((,c :inherit swiper-match-face-1)))
        `(swiper-background-match-face-2 ((,c :inherit swiper-match-face-2)))
        `(swiper-background-match-face-3 ((,c :inherit swiper-match-face-3)))
        `(swiper-background-match-face-4 ((,c :inherit swiper-match-face-4))))))))
(ef-themes-select 'ef-dream)

(provide 'veg-theming)
;;; veg-theming.el ends here
