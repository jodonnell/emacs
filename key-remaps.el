;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL KEY MAPPINGS
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-z"     'advertised-undo)
(global-set-key "\C-ch"    'help-command)
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-g"     'goto-line)
(global-set-key "\C-xt"    'toggle-truncate-lines)
(global-set-key "\C-xa"    'find-file-at-point-with-line)
(global-set-key "\C-xp"    'shell)
(global-set-key "\C-x\C-p" 'rename-buffer)
(global-set-key [f8]       'sw-list)
(global-set-key "\C-cp"    'new-shell)
(global-set-key "\C-x\C-y" 'yank-regexp)
(global-set-key (kbd "C-\\")   'indent-region)

(defun other-window-back()
  (interactive)
  (other-window -1))
(global-set-key "\C-x\C-o" 'other-window-back)

(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-b ?\C-c)
(keyboard-translate ?\C-c ?\C-b)
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'forward-char)

(global-set-key (kbd "C-.") 'kill-ring-save)

(global-unset-key (kbd "M-n"))
(global-set-key (kbd "M-n") 'forward-word)

(global-unset-key (kbd "C-h"))
(global-set-key (kbd "C-h") 'backward-char)

(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'backward-word)

(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t") 'next-line)

(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t") 'scroll-up)

(global-unset-key (kbd "C-b"))
(global-set-key (kbd "C-b") 'previous-line)

(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'scroll-down)

(global-unset-key (kbd "C-p"))
(global-set-key (kbd "C-p") 'delete-backward-char)

(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'hippie-expand)

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'universal-argument)

(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'universal-argument)

(global-set-key (kbd "C-c e") 'erase-buffer)


(global-set-key (kbd "s-a") 'erase-buffer)
(global-set-key (kbd "H-a") 'erase-buffer)
(global-set-key (kbd "A-a") 'erase-buffer)

(global-set-key (kbd "s-c") 'beginning-of-defun)
(global-set-key (kbd "s-t") 'end-of-defun)
(global-set-key (kbd "s-n") 'forward-sexp)
(global-set-key (kbd "s-h") 'backward-sexp)
(global-set-key (kbd "s-d") 'kill-sexp)

(global-set-key "\C-\M-n" 'forward-sexp)
(global-set-key "\C-\M-h" 'backward-sexp)
(global-set-key "\C-\M-c" 'backward-up-list)
(global-set-key "\C-\M-t" 'down-list)

(global-set-key "\C-x\C-f" 'helm-projectile)
(global-set-key "\C-xf" 'ido-find-file)
