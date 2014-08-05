(defvar django-tests-is-running nil)
(defvar django-tests-collected-output "")
(defvar django-tests-shell-buffer-name "django-tests")

(defun django-tests-check-done-running-tests (output)
  (if (and django-tests-is-running (string-match "^------" output))
      (progn
        (setq django-tests-is-running nil))))

(defun django-tests-build-string (str)
  (let ((output ""))
    (dotimes (i (length str) output)
      (let ((char (string (aref str i))))
        (setq output (concat output
                             (if (string-match "\\." char)
                                 (propertize char 'face '(:foreground "green"))
                               (propertize char 'face '(:foreground "red")))))))))
    
(defun django-tests-message-tests (output)
  (setq django-tests-collected-output (concat django-tests-collected-output output))
  (message "%s" (django-tests-build-string django-tests-collected-output)))

(defun django-tests-failed ()
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer django-tests-shell-buffer-name))

(defun django-tests-parse-output (process output)
  (set-buffer django-tests-shell-buffer-name)
  (insert (ansi-color-apply output))

  (django-tests-check-done-running-tests output)

  (if django-tests-is-running
      (django-tests-message-tests output))

  (if (string-match "^nosetests.*" output)
      (setq django-tests-is-running t))

  (if (string-match "FAILED (failures=.*)" output)
      (django-tests-failed)))

(defun django-tests-run ()
  "runs the gp renewables tests"
  (interactive)

  (setq django-tests-is-running nil)
  (setq django-tests-collected-output "")

  (let ((cb (current-buffer)))
    (shell django-tests-shell-buffer-name)
    (erase-buffer)
    (insert (concat "source ~/programming/gprenewables/virt/bin/activate; cd ~/programming/gprenewables/gp-dashboard/; REUSE_DB=1 ./manage.py test"))
    (comint-send-input)
    (switch-to-buffer cb)
    (set-process-filter (get-buffer-process django-tests-shell-buffer-name) 'django-tests-parse-output)))
