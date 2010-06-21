(defun unique-list ()
  "Takes a list in a buffer and removes all duplicates"
  (interactive)
  ;; create scratch buffer
  (let (cur-buf)
    (setq cur-buf (current-buffer))
    (get-buffer-create "dup_scratch_buffer")
    
    (while (buffer-size)
      (beginning-of-buffer)
      ;;save excursion
      (let (begin end text regex)
        ;; get region
        (setq begin (point))
        (move-end-of-line nil)
        (setq end (point))
        ;; save copy of text and then kill it
        (setq text (extract-rectangle begin end))
        (kill-region begin end)
        (delete-char 1) ;; remove blank line
        
        ;; put text in the unique buffer
        (set-buffer "dup_scratch_buffer")
        (yank)
        (newline)

        ;; remove all duplicates
        (set-buffer cur-buf)
        (setq regex (concat "^" (car text) "
")) ;; new line
        (replace-regexp regex "")
        
        )))
;; (kill-buffer "dup_scratch_buffer")

;; MAKE A FUNCTION THAT RENAMES SHELL AND CREATES A NEW ONE