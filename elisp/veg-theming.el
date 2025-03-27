;;; veg-theming.el --- my personal theme configuration using the Ef-themes.

;;; Code:

(require-theme 'modus-themes)

(setq modus-themes-mixed-fonts t
      modus-themes-org-blocks 'default
      modus-themes-to-toggle '(modus-vivendi modus-operandi-tinted)
      modus-themes-italic-constructs nil
      modus-themes-bold-constructs t
      modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)

(let ((mono-spaced-font "Aporetic Sans Mono")
	(proportionally-spaced-font "Aporetic Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 160)
  (set-face-attribute 'variable-pitch nil :family proportionally-spaced-font :height 1.0)

    (defun my-modus-themes-custom-faces ()
    "My customizations on top of the Modus themes.
This function is added to the `modus-themes-post-load-hook'."
    (modus-themes-with-colors
      (custom-set-faces
       ;;; core
       `(fringe ((,c :background ,bg-main)))
       `(line-number ((,c :background ,bg-main)))
       ;; highlight-indentation-mode
       ;; `(highlight-indentation-face ((,c :background ,bg-main)))
       ;; dashboard
       `(dashboard-text-banner ((,c :weight bold :foreground ,magenta-faint)))
       `(dashboard-heading ((,c :weight bold :foreground ,magenta-faint)))
       ;; ivy
       `(ivy-modified-buffer ((,c :foreground ,magenta)))
       `(ivy-org ((,c :foreground ,blue-faint :weight normal)))
       `(ivy-subdir ((,c :foreground ,cyan-faint)))
       ;; mood-line
       `(mood-line-buffer-status-modified ((,c :foreground ,fg-alt)))
       ;; olivetti-mode
       `(olivetti-fringe ((,c :background ,bg-main)))
       ;; org
       `(org-block ((,c :background ,bg-dim :extend t)))
       `(org-tag ((,c :foreground ,fg-dim :weight bold)))))))

(global-set-key (kbd "C-c m") 'modus-themes-toggle)

;; Using the hook lets our changes persist when we use the commands
;; `modus-themes-toggle' and `modus-themes-select'.
(add-hook 'modus-themes-post-load-hook #'my-modus-themes-custom-faces)
(modus-themes-select 'modus-vivendi)

(load-theme 'modus-vivendi)

(provide 'veg-theming)
