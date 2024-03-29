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
           (hippie-expand arg)))
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

(defun replace-all (dir wildcard replace with)
 (interactive "DDir: \nsFile wildcard: \nsReplace: \nsWith: ")
 (find-name-dired dir wildcard)
 (sleep-for 2)
 (dired-toggle-marks)
 (sleep-for 2)
 (dired-do-query-replace-regexp replace with)
 (save-some-buffers))



(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))

(defun restart-server()
  (interactive)
  (set-buffer "server")
  (comint-kill-subjob)
  (shell-insert-send-sleep "rails s" 0))

(defun js-make-private ()
  (interactive)
  (let ((token (thing-at-point 'word)))
    (save-excursion
      (beginning-of-buffer)
      (query-replace token (concat "_" token)))))


(defun get-image-size ()
  (interactive)
  (let ((extension (file-name-extension (buffer-file-name))))
    (if (or (string= extension "png") (string= extension "jpg") (string= extension "gif"))
        (get-image-size-from-file (buffer-file-name))
      (get-image-size-from-file (read-file-name "File name: ")))))

(defun get-image-size-from-file (file-name)
  (let ((image-size (shell-command-to-string (concat "identify -format '%w %h' " file-name))))
       (message
        (concat
         "width: "
         (car (split-string image-size))
         " height: "
         (car (cdr (split-string image-size)))))))

(defun google(query)
  (interactive "sQuery: ")
  (eww (concat "http://www.google.com/search?gbv=1&source=hp&hl=en&ie=ISO-8859-1&btnG=Google+Search&q=" query)))

(defun wrap-lines-region-html (b e tag)
  "'tag' every line in the region with a tag"
  (interactive "r\nMTag for line: ")
  (setq p (point-marker))
  (save-excursion
    (goto-char b)
    (while (< (point) p)
      (beginning-of-line)
      (indent-according-to-mode)
      (insert (format "<%s>" tag))
      (end-of-line)
      (insert (format "</%s>" tag))
      (forward-line 1))))

(defun insert-tag-at-position-and-indent (start format-tag)
  (goto-char start)
  (beginning-of-line)
  (indent-according-to-mode)
  (insert (format format-tag tag))
  (newline))

(defun wrap-region-html (b e tag)
  "'tag' every line in the region with a tag"
  (interactive "r\nMTag for line: ")
  (setq p (point-marker))
  (save-excursion
    (insert-tag-at-position-and-indent b "<%s>")
    (insert-tag-at-position-and-indent p "</%s>")
    (indent-region b p)
    (beginning-of-line)
    (indent-according-to-mode)))


(defun rdio-play ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to playpause'"))

(defun rdio-next ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to next track'"))

(defun rdio-previous ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to previous track'"))


(defun sacha/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sacha/smarter-move-beginning-of-line)


(defun apropos-at-point ()
  (interactive)
  (apropos (thing-at-point 'symbol)))


(defun indent-jsx (beg end)
  (interactive "r")
  (kill-region beg end)
  (save-excursion
    (let ((buffer-name (generate-new-buffer-name "poop")))
      (get-buffer-create buffer-name)
      (set-buffer buffer-name)
      (yank)
      (html-mode)
      (goto-char (point-min))
      (forward-line 1)
      (indent-region (point) (point-max))
      (kill-region (point-min) (point-max))))
  (yank))

(defun eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      eslint)))

(defun eslint-fix-current-file()
  (interactive)
  (shell-command-to-string (concat (eslint-from-node-modules) " " (buffer-file-name) " --fix")))



;; for opening es6 imports easily
(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2017-09-01"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                                ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (progn
              (find-file (xahsite-url-to-filepath $path)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
            (progn
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $fpath))
                      (find-file $fpath))))))
          (progn
            (if (file-exists-p $path)
                (find-file $path)

              (cond ((file-exists-p (concat $path ".el")) (find-file (concat $path ".el")))
                    ((file-exists-p (concat $path ".js")) (find-file (concat $path ".js")))
                    ((file-exists-p (concat $path ".jsx")) (find-file (concat $path ".jsx")))
                    (t
                     (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $path))
                       (find-file $path )))))))))))

(global-set-key (kbd "C-c o") 'xah-open-file-at-cursor)
