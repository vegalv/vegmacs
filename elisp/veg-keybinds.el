;;; veg-keybinds.el --- global keybindings.

;;; Core
;; Switching windows
(global-set-key (kbd "C-x p") (lambda ()
                                (interactive)
                                (other-window 1)))

(global-set-key (kbd "C-x o") (lambda ()
                                (interactive)
                                (other-window -1)))

;; Use C-z for undo instead of suspend-frame
(global-set-key (kbd "C-z") 'undo)

;; Delete words without copying to kill-ring
(global-set-key (kbd "M-d") 'veg/delete-word)
(global-set-key (kbd "<C-backspace>") 'veg/backward-delete-word)

(global-set-key (kbd "M-i") 'join-line)
(global-set-key (kbd "M-`") 'beginning-of-buffer)

(global-set-key (kbd "M-~") 'end-of-buffer)
(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)

;; Org links
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link) ;; FIXME does not need to be global

;; TODO Move to veg-org.el
;; (define-key org-mode-map (kbd "C-c æ") 'org-edit-special)
;; (define-key org-src-mode-map (kbd "C-c æ") 'org-edit-src-exit)
;; (define-key org-src-mode-map (kbd "C-c k") 'org-edit-src-abort)

(provide 'veg-keybinds)
;;; veg-keybinds.el ends here
