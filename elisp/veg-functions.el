;; Deleting words without copying to kill-ring
;; (https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring)
(defun veg/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun veg/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With  this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))


(provide 'veg-functions)
