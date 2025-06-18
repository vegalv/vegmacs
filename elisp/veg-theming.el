;;; veg-theming.el --- my personal theme configuration on top of the Modus themes.

;;; Code:

(require-theme 'modus-themes)

(setq modus-themes-mixed-fonts t
      modus-themes-org-blocks 'default
      modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted)
      modus-themes-bold-constructs t
      modus-vivendi-tinted-palette-overrides '((comment fg-dim)))

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
       ;; core
       `(fringe ((,c :background ,bg-main)))
       `(line-number ((,c :background ,bg-main)))
       ;; dashboard
       `(dashboard-text-banner ((,c :weight bold :foreground ,magenta-faint)))
       `(dashboard-heading ((,c :weight bold :foreground ,magenta-faint)))
       ;; font-lock
       `(font-lock-variable-name-face ((,c :foreground ,fg-main :slant italic)))
       `(font-lock-type-face ((,c :foreground ,fg-alt :weight bold :slant normal)))
       ;; git
       `(git-commit-summary ((,c :foreground ,fg-alt :weight bold)))
       ;; ivy
       `(ivy-modified-buffer ((,c :foreground ,magenta)))
       `(ivy-org ((,c :foreground ,blue-faint :weight normal)))
       `(ivy-subdir ((,c :foreground ,cyan-faint)))
       ;; mood-line
       `(mood-line-buffer-status-modified ((,c :foreground ,fg-alt)))
       ;; olivetti-mode
       `(olivetti-fringe ((,c :background ,bg-main)))
       ;; org
       `(org-agenda-date ((,c :foreground ,fg-alt :weight bold)))
       `(org-agenda-date-today ((,c :foreground ,fg-alt :underline ,fg-alt :weight bold)))
       `(org-agenda-calendar-event ((,c :foreground ,fg-alt :slant italic)))
       `(org-agenda-date-weekend ((,c :foreground ,fg-main)))
       `(org-date ((,c :foreground ,fg-alt)))
       `(org-deadline ((,c :foreground ,red-faint)))
       `(org-imminent-deadline ((,c :foreground ,red-faint)))
       `(org-upcoming-deadline ((,c :foreground ,red-faint)))
       `(org-upcoming-distant-deadline ((,c :foreground ,red-faint)))
       `(org-scheduled ((,c :foreground ,fg-alt)))
       `(org-scheduled-previously ((,c :foreground ,fg-alt)))
       `(org-block ((,c :background ,bg-dim :extend t)))
       `(org-tag ((,c :foreground ,fg-dim :weight bold)))
       `(org-done ((,c :foreground ,green-cooler)))
       `(org-headline-done ((,c :foreground ,fg-dim)))
       `(org-todo ((,c :foreground ,red-cooler)))
       `(org-verbatim ((,c :foreground ,magenta-faint)))
       `(org-document-title ((,c :foreground ,fg-alt :weight bold)))))))

(global-set-key (kbd "C-c m") 'modus-themes-toggle)

;; Using the hook lets our changes persist when we use the commands
;; `modus-themes-toggle' and `modus-themes-select'.
(add-hook 'modus-themes-post-load-hook #'my-modus-themes-custom-faces)
(modus-themes-select 'modus-vivendi-tinted)

(provide 'veg-theming)
;; veg-theming.el ends here
