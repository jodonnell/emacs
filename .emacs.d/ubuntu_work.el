;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP STUFF
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
	     '("\\." nil "/ssh:jodonnell@devt1:"))


(require 'magit)

(defun work-start-up ()
  "Starts up all the shell buffers I need for work"
  (interactive)
  (shell)
  (rename-buffer "log")

  (shell)
  (rename-buffer "sql")

  (shell)
  (rename-buffer "release")

  (shell)
  (rename-buffer "httpd")

  (shell)
  
  (sleep-for 3)
  
  (set-buffer "log")
  (shell-insert-send-sleep "ssh devt1" 2)
  (shell-insert-send-sleep "ssh p-devv1.sgc" 2)
  (shell-insert-send-sleep "tail -f cm_develop/var/error" 3)


  (set-buffer "sql")
  (shell-insert-send-sleep "ssh devt1" 2)
  (shell-insert-send-sleep "ssh p-devv1.sgc" 2)
  (shell-insert-send-sleep "sqlplus classic_sgc/classic_sgc123" 3)
  (shell-insert-send-sleep "set linesize 32767;" 1)
  (toggle-truncate-lines)


  (set-buffer "release")
  (shell-insert-send-sleep "ssh devt1" 2)
  (insert "/data/cheetah/bin/release --env vm --vm-project=sgc --notify=jodonnell --git-root=projects --git-project=develop --git-branch=m_and_e/sgc_integration")


  (set-buffer "httpd")
  (shell-insert-send-sleep "ssh devt1" 2)
  (shell-insert-send-sleep "ssh p-devv1.sgc" 2)
  (shell-insert-send-sleep "sudo su - httpd" 3)
  (shell-insert-send-sleep "cd /home/jodonnell" 2)
  (shell-insert-send-sleep "export HOME=/home/jodonnell" 2)
  (insert "./ide_ctl restart cm_develop")


  (set-buffer "*shell*")
  (shell-insert-send-sleep "ssh devt1" 2)
  (shell-insert-send-sleep "ssh p-devv1.sgc" 2)
  (insert "/ssh:jodonnell@p-devv1.sgc:")

  (find-file "~/.emacs"))


(work-start-up)

(defun perltidy-region ()
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

