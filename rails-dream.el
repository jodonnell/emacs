;;;;;;;;;;
;; process filter functions
(defun remove-shell-artifacts (output)
  (defun remove-all-in-current-buffer (from)
    (beginning-of-buffer)
    (while (search-forward from nil t)
      (replace-match "" nil t)))

  (insert (ansi-color-apply output))
  (font-lock-mode)
  (remove-all-in-current-buffer "\r")
  (remove-all-in-current-buffer ">>"))

(defun ri-arguments-collect-and-cleanse-output (process output)
  (set-buffer output-buffer)
  (if (string-match "Nothing known about" output)
      (message "You need to have buffer created")) ; do something
  (if (string-match ">>" output)
      (progn 
        (remove-shell-artifacts output)

        (beginning-of-buffer)
        (kill-line 3)
        
        (search-forward (concat function-to-get-arguments "(") nil t)t
        (let ((begin-of-methods-pos (point)))
          (search-forward ")" nil t)
          (backward-char)
          (message (buffer-substring begin-of-methods-pos (point)))))

    (insert (ansi-color-apply output))))

(defun ri-collect-and-cleanse-output (process output)
  (set-buffer output-buffer)
  (if (string-match "Nothing known about" output)
      (message "You need to have buffer created")) ; do something
  (if (string-match ">>" output)
      (progn 
        (remove-shell-artifacts output)

        (switch-to-buffer output-buffer)
        (beginning-of-buffer)
        (kill-line 3))

    (insert (ansi-color-apply output))))

(defun rails-collect-and-cleanse-output (process output)
  (set-buffer output-buffer)
  (if (string-match "Nothing known about" output)
      (message "You need to have buffer created"))
  (if (string-match "nil]" output)
      (progn 
        (remove-shell-artifacts output)

        (switch-to-buffer output-buffer)
        (beginning-of-buffer)
        (kill-line 1)
        (end-of-buffer)
        (previous-line 3)
        (beginning-of-line)
        (kill-line 3)
        (beginning-of-buffer))

    (insert (ansi-color-apply output))))

;;;;;;;;;;
;; buffer creation functions
(defun create-or-empty-output-buffer (buffer-name)
  (set-buffer (get-buffer-create buffer-name))
  (erase-buffer)
  (setq output-buffer buffer-name))

(defun create-console-buffer-if-does-not-exist (buffer-name command)
  (defun open-console-buffer ()
    (create-console-buffer)
    (sit-for 1)
    (process-send-string buffer-name command)
    (sit-for 8))

  (defun create-console-buffer ()
    (let ((old-buffer (current-buffer)))
      (shell buffer-name)
      (switch-to-buffer old-buffer)))

  (if (not (get-buffer buffer-name))
      (open-console-buffer)))

;;;;;;;;;;
;; method getter functions
(defun get-instance-methods (class-name)
  (get-methods class-name "instance_methods"))

(defun get-class-methods (class-name)
  (get-methods class-name "methods"))

(defun get-class-methods-current ()
  (interactive)
  (get-class-methods (get-class)))

(defun get-instance-methods-current ()
  (interactive)
  (get-instance-methods (get-class)))

(defun get-class ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "class[ \t]*")
    (thing-at-point 'word)))

(defun get-methods (class-name method-type)
  (save-excursion
    (create-or-empty-output-buffer (concat class-name " " method-type))
    (create-console-buffer-if-does-not-exist "rails-console-buffer" "rails c\n")

    (set-process-filter (get-buffer-process "rails-console-buffer") 'rails-collect-and-cleanse-output)
    (process-send-string "rails-console-buffer" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
    (bury-buffer "rails-console-buffer")))

;;;;;;;;;;
;; documentation functions
(defun get-rails-documentation ()
  (interactive)
  (get-rails-documentation-for-function (thing-at-point 'symbol)))

(defun get-rails-documentation-for-function (function)
  (save-excursion
    (create-or-empty-output-buffer (concat function "-documentation"))
    (create-console-buffer-if-does-not-exist "ri-console-buffer" "ri -i -T\n")

    (set-process-filter (get-buffer-process "ri-console-buffer") 'ri-collect-and-cleanse-output)
    (process-send-string "ri-console-buffer" (concat function "\n"))
    (bury-buffer "ri-console-buffer")))

;;;;;;;;;;
;; arguments functions
(defun get-rails-function-argument-list-at-point ()
  (interactive)
  (get-rails-function-argument-list (thing-at-point 'symbol)))

(defun get-rails-function-argument-list (function) 
  (save-excursion
    (create-or-empty-output-buffer (concat function "-documentation"))
    (setq function-to-get-arguments function)
    (create-console-buffer-if-does-not-exist "ri-console-buffer" "ri -i -T\n")

    (set-process-filter (get-buffer-process "ri-console-buffer") 'ri-arguments-collect-and-cleanse-output)
    (process-send-string "ri-console-buffer" (concat function "\n"))
    (bury-buffer "ri-console-buffer")))

(provide 'rails-dream)
