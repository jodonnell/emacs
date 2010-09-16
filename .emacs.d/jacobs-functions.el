(defun unique-list ()
  "Takes a list in a buffer and removes all duplicates"
  (interactive)
  ;; create scratch buffer
  (let (cur-buf)
    (setq cur-buf (current-buffer))
    (get-buffer-create "dup_scratch_buffer")
    
    (while (> (buffer-size) 0)
      (beginning-of-buffer)
      
      (let (begin end text regex)
        
        ;; get region
        (setq begin (point))
        (move-end-of-line nil)
        (setq end (point))
        
        ;; save copy of text and then kill it
        (setq text (extract-rectangle begin end))
        (kill-region begin end)
        (if (> (buffer-size) 0)
            (delete-char 1)) ;; remove blank line
        
        ;; put text in the unique buffer
        (set-buffer "dup_scratch_buffer")
        (yank)
        (newline)

        ;; remove all duplicates
        (set-buffer cur-buf)
        (setq regex (concat "^" (car text) "
")) ;; new line
        (replace-regexp regex "")))
    
    (set-buffer cur-buf)
    (insert-buffer "dup_scratch_buffer")
    (kill-buffer "dup_scratch_buffer")))

(defun new-shell ()
  "Opens a new shell, if one already existed rename it uniquely"
  (interactive)
  (if (get-buffer "shell")
    (set-buffer "shell")
    (rename-uniquely))
  (shell))


(setq yank-regexp-last-regex "")
(setq yank-regexp-last-yank "")
(setq yank-regexp-last-kill-ring-index 0)
(setq yank-regexp-last-point-pos 0)

(defun yank-regexp-set-vars (regexp kill-ring-entry kill-ring-index)
      (setq yank-regexp-last-regex regexp)
      (setq yank-regexp-last-yank kill-ring-entry)
      (setq yank-regexp-last-kill-ring-index (+ kill-ring-index 1))
      (setq yank-regexp-last-point-pos (point)))

(defun yank-regexp (regexp)
  "Finds the last yank used that matches a regexp"
  (interactive "sYank Regexp: ")
  (block yank-regexp
  (if (equal regexp "")
      (setq regexp yank-regexp-last-regex))

    (let ((kill-ring-index 0))
      ;; if yank-regexp-last-regex == current  && yank-regexp-last-point-pos == current point
      (if (and (equal yank-regexp-last-regex regexp) (equal yank-regexp-last-point-pos (point)))
    (progn
      ;;    restart seach from yank-regexp-last-kill-ring-entry
      (setq kill-ring-index yank-regexp-last-kill-ring-index)
      ;; remove yank-regexp-last-yank which should be right behind point
      (let ((end (point)))
        (search-backward yank-regexp-last-yank)
        (delete-region (point) end))))
      (while (> kill-ring-max kill-ring-index)
    (setq kill-ring-entry (current-kill kill-ring-index 1))
    (when (string-match regexp kill-ring-entry)
      (insert kill-ring-entry)
      (yank-regexp-set-vars regexp kill-ring-entry kill-ring-index)
      (return-from yank-regexp))
    (setq kill-ring-index (+ kill-ring-index 1))))))


(defun th-complete-or-indent (arg)
  "If preceding character is a word character and the following
character is a whitespace or non-word character, then
`dabbrev-expand', else indent according to mode."
  (interactive "*P")
  (cond ((and
          (= (char-syntax (preceding-char)) ?w)
          (looking-at (rx (or word-end (any ".,;:#=?()[]{}")))))
         (require 'sort)
         (let ((case-fold-search t))
           (dabbrev-expand arg)))
         (t
          (indent-according-to-mode))))

(defun dev-cd ()
  "Opens a shell on the dev server and switches cd to the dev home dir"
  (interactive)
  (cd "/ssh:jodonnell@devv1.cla:")
  (shell)
  (sleep-for 3)
  (insert "ssh devt1")
  (comint-send-input)
  (sleep-for 3)
  (insert "ssh devv1.cla")
  (comint-send-input)
  )

(defun shell-insert-send-sleep (command sleep)
  (insert command)
  (comint-send-input)
  (sleep-for sleep))


(defun push-bugfix (bz)
  "Finds the last yank used that matches a regexp"
  (interactive "sBZ: ")
  (block push-bugfix
    (shell)
    (sleep-for 1)

    (shell-insert-send-sleep "git log -n 1 | cat" 2)

    (search-backward "commit")
    (forward-char 7)

    (setq begin (point))
    (move-end-of-line nil)
    (setq end (point))
        
    (setq commit (buffer-substring-no-properties begin end))
    (end-of-buffer)

    (shell-insert-send-sleep "git push origin slt_rewrite:m_and_e/slt_rewrite" 2)

    (shell-insert-send-sleep "git checkout trunk/release" 2)

    (shell-insert-send-sleep "git pull trunk rc" 3)

    (shell-insert-send-sleep "git merge slt_rewrite" 2)

    (search-backward "Merge made by recursive.")
    (next-line)
    (beginning-of-line)

    (setq begin (point))
    (search-forward "files changed,")
    (move-end-of-line nil)
    (setq end (point))
    (setq files-changed (buffer-substring-no-properties begin end))
    (end-of-buffer)

    (setq branch-name (concat "bugfix/bz" bz))
    (shell-insert-send-sleep (concat "git push origin trunk/release:" branch-name) 2)

    (shell-insert-send-sleep "git checkout slt_rewrite" 1)

    (get-buffer-create "dup_scratch_buffer")
    (switch-to-buffer "dup_scratch_buffer")
    (erase-buffer)
    (insert commit)
    (newline)
    (insert branch-name)
    (newline)
    (insert files-changed)
    (newline)))


;; (defadvice ido-switch-buffer (after record-time-in-buffer)
;;   "When you switch to a buffer records the time spent in that buffer"
;;   (setq buffer-name (buffer-name))
;;   (setq buffer (current-buffer))
;;   (find-file "~/time.org")
;;   (org-clock-out 1)
;;   (beginning-of-buffer)

;;   (if (search-forward buffer-name nil t)
;;       nil
;;     (progn
;;       (end-of-buffer)
;;       (insert (concat "
;; ** " buffer-name))))
     
;;   (org-clock-in)
;;   (bury-buffer "time.org")
;;   (switch-to-buffer buffer))

;(ad-activate 'ido-switch-buffer)