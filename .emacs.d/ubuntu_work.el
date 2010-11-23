;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP STUFF
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
	     '("\\." nil "/ssh:jodonnell@devt1:"))


(defun ssh-devt1 ()
  (shell-insert-send-sleep "ssh devt1" 2))

(defun ssh-dev-env (env)
  (shell-insert-send-sleep (concat "ssh " env) 2))

(defun work-start-up (env)
  "Starts up all the shell buffers I need for work"
  (interactive)
  (shell "log")
  (shell "sql")
  (shell "release")
  (shell "httpd")
  (shell)
  
  (sleep-for 3)
  
  (set-buffer "log")
  (ssh-devt1)
  (ssh-dev-env env)
  (if (equal "env" "sgc")
      (shell-insert-send-sleep "tail -f cm_develop/var/error" 3)
    (shell-insert-send-sleep "tail -f cm_develop/var/error" 3))

  (set-buffer "sql")
  (ssh-devt1)
  (ssh-dev-env env)
  (shell-insert-send-sleep (concat "sqlplus classic_" env "/classic_" env "123") 3)
  (shell-insert-send-sleep "set linesize 32767;" 1)
  (toggle-truncate-lines)


  (set-buffer "release")
  (ssh-devt1)
  (insert "/data/cheetah/bin/release --env vm --vm-project=sgc --notify=jodonnell --git-root=projects --git-project=develop --git-branch=m_and_e/sgc_integration")


  (set-buffer "httpd")
  (ssh-devt1)
  (ssh-dev-env env)
  (shell-insert-send-sleep "sudo su - httpd" 3)
  (shell-insert-send-sleep "cd /home/jodonnell" 2)
  (shell-insert-send-sleep "export HOME=/home/jodonnell" 2)
  (insert "./ide_ctl restart cm_develop")


  (set-buffer "*shell*")
  (ssh-devt1)
  (ssh-dev-env env)
  (insert (concat "/ssh:jodonnell@" env ":"))

  (find-file "~/.emacs"))


;(work-start-up "devv1.cla")

(defun perltidy-region ()
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region (point) (mark) "perltidy -q" nil t)))


(defun perl-syntax-get-syntax-error-line ()
  (save-excursion
      (set-buffer "*Shell Command Output*")
      (goto-char (point-min))
      (if (looking-at ".*line \\([0-9]+\\)")
	  (string-to-number (match-string-no-properties 1))
	nil)))
  
(defun perl-syntax-check ()
    "Checks to see if your perl program compiles"
    (interactive)

    (save-excursion
      (shell-command-on-region (goto-char (point-min)) (goto-char (point-max)) "perl -cW"))

    (let syntax-error-line (perl-syntax-get-syntax-error-line)
	 (if syntax-error-line
	     (progn
	       (goto-char (point-min))
	       (goto-line (syntax-error-line))
	       (set-buffer "*Shell Command Output*")
	       (message (buffer-string))))))


;; (add-hook 'after-save-hook (lambda() 
;; 			     (setq current-file-extension (file-name-extension (buffer-file-name)))
;; 			     (if (or (string= "pl" current-file-extension) (string= "pm" current-file-extension))
;; 				 (perl-syntax-check))))



(defun js-run-tests ()
  (interactive)

  (shell "js-test")
  (erase-buffer)
  (shell-insert-send-sleep "java -jar JsTestDriver-1.2.2.jar --tests all" 1))

(js-run-tests)
  