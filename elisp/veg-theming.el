;;; veg-theming.el --- my personal theme configuration using the Modus themes.

;;; Code:

(require-theme 'modus-themes)

(setq modus-themes-mixed-fonts t
      modus-themes-org-blocks 'default
      modus-themes-to-toggle '(modus-vivendi modus-operandi-tinted)
      modus-themes-italic-constructs nil
      modus-themes-bold-constructs t)

(let ((mono-spaced-font "Aporetic Sans Mono")
	(proportionally-spaced-font "Aporetic Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 150)
  (set-face-attribute 'variable-pitch nil :family proportionally-spaced-font :height 1.0))


(load-theme 'modus-vivendi)


(provide 'veg-theming)
