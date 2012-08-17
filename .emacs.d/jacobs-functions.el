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

(defun th-complete-or-indent2 (arg)
  "If preceding character is a word character and the following
character is a whitespace or non-word character, then
`dabbrev-expand', else indent according to mode."
  (interactive "*P")
  (cond ((and
          (= (char-syntax (preceding-char)) ?w)
          (looking-at (rx (or word-end (any ".,;:#=?()[]{}")))))
         (require 'sort)
         (let ((case-fold-search t))
           (auto-complete)))
         (t
          (indent-according-to-mode))))

(defun shell-insert-send-sleep (command sleep)
  (insert command)
  (comint-send-input)
  (sleep-for sleep))


(defun git-bisect (good)
  (interactive "sLast good: ")
  (git-bisect)
  (shell "shell-change")
  (sleep-for 3)
  (shell-insert-send-sleep "cd ~/cm_develop" 2)
  
  (shell-insert-send-sleep "git bisect start" 3)
  (shell-insert-send-sleep "git bisect bad" 4)
  (shell-insert-send-sleep (concat "git bisect good " good) 4)

  (while (eq (search-backward "is the first bad commit" (point-min) t) nil)
    (erase-buffer)
    (shell-insert-send-sleep "perl -I lib/ lib/CTAH/Mailing/Preview/tests.t" 12)
    
    (if (eq (search-backward "not ok [0-9]+" (point-min) t) nil)
	(shell-insert-send-sleep "git bisect bad" 4)
      (shell-insert-send-sleep "git bisect good" 4)))
  
  (shell-insert-send-sleep "git bisect reset" 2))


(defun refactor-extract-method(start end)
  (interactive r)

  (defun indent-our-region()
    (save-excursion
      (search-backward-regexp "^sub" nil t)
      (beginning-of-line)
      (let ((begin (point)))
	(search-forward-regexp "}" nil t)
	(indent-region begin (point)))))
  

  (defun get-and-delete-old-code (start end)
    (setq new-method-body (buffer-substring-no-properties start end))
    (delete-region start end))


  (defun move-to-beginning-of-next-sub ()
    (search-forward-regexp "^sub" nil t)
    (beginning-of-line))

  (defun create-new-method ()
    (insert "sub {\n\n}\n\n")
    (previous-line 3)
    (insert new-method)
    (indent-our-region))

  (defun find-vars (new-method-body))
    ;; search to bound till end of method
    ;;looking for $, @, %
    ;;take these symbols and look for a prefix of my, our, local
    ;;my x or my (x,y,z)
    ;;the rest are parameters
    ;;rename method and parameters.
    ;;pass in papa paramaters


  (setq new-method-body (get-and-delete-old-code start end))
  (move-to-beginning-of-next-sub)
  (create-new-method)

  (find-vars new-method-body)
)

(defun rubymotion (command)
  (if (get-buffer "rubymotion")
      (kill-buffer "rubymotion"))
  (shell "rubymotion")
  (process-kill-without-query (get-buffer-process "rubymotion"))
  (insert (concat "cd ~/programming/FallingChars; rake " command))
  (comint-send-input))

(defun rubymotion-simulator ()
  (interactive)
  (rubymotion ""))

(defun rubymotion-spec ()
  (interactive)
  (rubymotion "spec"))

(defun rubymotion-device ()
  (interactive)
  (rubymotion "device"))

(defun replace-all (dir wildcard replace with)
 (interactive "DDir: \nsFile wildcard: \nsReplace: \nsWith: ")
 (find-name-dired dir wildcard)
 (sleep-for 2)
 (dired-toggle-marks)
 (sleep-for 2)
 (dired-do-query-replace-regexp replace with)
 (save-some-buffers))



(defun kill-lua-tests (process output)
  (insert (ansi-color-apply output))
  (if (string-match "Simulation Terminated: Lua script called os.exit() with status" output)
      (comint-kill-subjob)))

(defun run-lua-tests ()
  "runs the lua corona tests for zombie run"
  (interactive)
  (shell "lua-tests")
  (erase-buffer)
  (insert "cd ~/programming/zombie_run/lua/; LUA_TEST=true /Applications/CoronaSDK/Corona\\ Terminal main.lua")
  (comint-send-input)
  (set-process-filter (get-buffer-process "lua-tests") 'kill-lua-tests))
