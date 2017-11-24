;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BB
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "proxy.bloomberg.com:81")
     ("https" . "proxy.bloomberg.com:81")))

(defun open-ticket()
  (interactive)
  (shell-command (concat "open https://pm.bloomberg.com/jira/browse/" (replace-regexp-in-string "feature/" "" (thing-at-point 'symbol)))))

(defun delete-ticket()
  (interactive)
  (shell-command (concat "git branch -d " (thing-at-point 'symbol))))


(defun copy-file-name-to-kill-ring ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
