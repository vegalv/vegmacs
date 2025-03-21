;;; veg-org.el --- my personal org-mode configuration.

;;; Code:

(require 'org)

;;; ================================================================================
;;;                          Base org-mode configuration
;;; --------------------------------------------------------------------------------

(add-hook 'org-mode-hook 'variable-pitch-mode) 

(add-hook 'org-mode-hook 'visual-line-mode) 

(setq org-hide-emphasis-markers t ; Disabling italics/bold/etc expression characters
      org-list-indent-offset 1
      org-pretty-entities t
      org-startup-with-inline-images t
      org-image-actual-width '(500)) ;; Inline image width

(setq org-support-shift-select t)

(setq org-src-fontify-natively t) 

(setq org-src-tab-acts-natively t) ; Tab indentation in source blocks

(setq org-tags-column 0) ; No tag alignment

(setq org-fontify-quote-and-verse-blocks t) 

(setq org-ellipsis "…") ; Ellipsis on folded headings

(setq org-pretty-entities-include-sub-superscripts nil)

(setq org-format-latex-options '(:foreground default :background default :scale 1.7 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                                             ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-goto-auto-isearch nil) ; Allows navigation with n/p/etc. with org-goto

(setq org-startup-folded 'fold)

(use-package org-superstar
  :ensure t
  :hook
  (org-mode . org-superstar-mode))

(use-package org-indent
  :hook
  (org-mode . org-indent-mode)
  :config
  ;; Set org-indent face to constant height to get proper height in variable-pitch-mode
  (set-face-attribute 'org-indent nil :height 160))

;;; ===============================================================================
;;;                                       org-babel
;;; -------------------------------------------------------------------------------

;; Loading languages for babel code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (C . t)
   (latex . t)
   (java . t)
   ;; (racket . t)
   (sql . t)))

;; Name of the command for executing Python code
;; (setq org-babel-python-command "python3")

;; Babel easier eval - so it doesn't always ask you yes or no
(setq org-confirm-babel-evaluate nil)

;; Org source editor behavior
(setq org-src-window-setup 'current-window)

;;; ================================================================================
;;;                               Agenda configuration
;;; Heavily inspired by N. P. Rougier's guide: https://github.com/rougier/emacs-GTD
;;; --------------------------------------------------------------------------------
;;; Key bindings
(define-key global-map    (kbd "C-c c")  'org-capture)
(define-key global-map    (kbd "C-c i")  'org-capture-inbox)
(define-key global-map    (kbd "C-c a")  'org-agenda)

(let ((agenda-dir "/Users/vegalv/org/agenda/"))
  (let ((inbox (concat agenda-dir "inbox.org"))
        (uiobox (concat agenda-dir "uiobox.org"))
        (notes (concat agenda-dir "notes.org"))
        (stash (concat agenda-dir "stash.org"))
        (recurrent (concat agenda-dir "recurrent.org"))
        (tasks (concat agenda-dir "tasks.org"))
        (calendar (concat agenda-dir "calendar.org"))
        (in3000-os "/Users/vegalv/in3000/in3000-os.org")
        (in2000-sw "/Users/vegalv/in2000/in2000-sw.org"))

    (setq org-agenda-files (list inbox
                                 uiobox
                                 notes
                                 stash
                                 recurrent
                                 tasks
                                 calendar
                                 in3000-os
                                 in2000-sw))
    ;;; Org-Capture
    (setq org-capture-templates
          `(("t" "Todo" entry  (file ,inbox)
             ,(concat "* TODO %?\n")) ; %U gives date, day and time

            ("e" "Event" entry (file+olp+datetree "~/org/agenda/calendar.org")
             "* %?\n %T" :time-prompt t :tree-type month)

            ("u" "UiO note" entry (file ,uiobox)
             ,(concat "* %? \n"))

            ("n" "Note" entry (file ,notes)
             ,(concat "* %? \n"))

            ("s" "Stash" entry (file ,stash)
             ,(concat "* %? \n"))))

    (add-hook 'org-capture-mode-hook 'delete-other-windows)

    ;;; Refile
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)

    (setq org-refile-targets
          `((tasks :maxlevel . 1)
            (notes :maxlevel . 1)
            (in2000-sw :maxlevel . 3)
            (in3000-os :maxlevel . 3)))

    ;; Automatic saving after refiling
    ;; Save the corresponding buffers
    (defun save-org-buffers ()
      "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
      (interactive)
      (message "Saving org-agenda-files buffers...")
      (save-some-buffers t (lambda ()
                             (when (member (buffer-file-name) org-agenda-files)
                               t)))
      (message "Saving org-agenda-files buffers... done"))

    ;; Add it after refile
    (advice-add 'org-refile :after
                (lambda (&rest _)
                  (save-org-buffers)))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "CANCELLED" "DONE(D)")))

;;; Agenda view
(setq org-deadline-warning-days 3)
(setq org-agenda-hide-tags-regexp ".") ;; hides ALL tags in agenda view. Can customize this if we want to
(setq org-agenda-skip-scheduled-if-done t)

;;; Custom agenda view
(setq org-agenda-custom-commands
      '(("g" "Main agenda"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-deadline-warning-days 3)
                   (org-agenda-get-deadlines)))

          (todo "NEXT"
                ((org-agenda-overriding-header "\nActive tasks")))

          (todo "WAITING"
                ((org-agenda-overriding-header "\nBlocked tasks")))

          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today"))))

         ((org-agenda-compact-blocks t)))

        ("h" "IN2000"
         ((tags-todo "in2000&general"
                     ((org-agenda-overriding-header "IN2000"))))
         ((org-agenda-compact-blocks t)))

        ("k" "IN3000 General"
         ((tags-todo "in3000&general"
                     ((org-agenda-overriding-header "IN3000 General"))))
         ((org-agenda-compact-blocks t)))

        ("l" "IN3000 Oblig"
         ((tags-todo "in2000&oblig"
                     ((org-agenda-overriding-header "IN3000 Oblig"))))
         ((org-agenda-compact-blocks t)))))

(setq org-agenda-restore-windows-after-quit t)

(provide 'veg-org)
;;; veg-org.el ends here
