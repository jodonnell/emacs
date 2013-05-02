(defvar extract-running nil)

(defvar extract-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name (file-name-directory current)))
  "Path to the backend Ruby code.")

(defun extract-method(method-name)
  (interactive "sNew method name: ")
  (when (not extract-running)
    (progn
      (setq extract-running t)
      (let ((script (format (mapconcat #'identity
                                       '("unless defined? ASTRefactor"
                                         "$:.unshift '%s'"
                                         "require 'extract'"
                                         "require 'ripper'"
                                         "ast_refactor = ASTRefactor.new"
                                         "end\n")
                                       "; ")
                            extract-ruby-path)))
        (comint-send-string (inf-ruby-proc) script))))
  (save-excursion 
    (replace-region-with-method method-name)
    (find-spot-to-insert-new-method)
    (insert-new-method method-name)
    (save-excursion
      (old-method-into-ripper))
    (new-method-into-ripper)
    (run-with-idle-timer 3 nil 'add-args method-name)))
    

(defun add-args(method-name)
  (save-excursion
    (let (args)
      (setq args (grab-arguments))
      (replace-string method-name (concat method-name " " args)))))

(defun grab-arguments()
  (save-excursion
    (set-buffer "*ruby*")
    (search-backward "=> ")
    (forward-char 4)
    (let (start)
      (setq start (point))
      (search-forward "\"")
      (backward-char)
      (buffer-substring-no-properties start (point)))))


(defun replace-region-with-method(method-name)
  (kill-region (point) (mark))
  (insert-and-indent method-name)
  (newline))

(defun find-spot-to-insert-new-method()
  (end-of-defun)
  (newline)
  (beginning-of-line))

(defun insert-new-method(method-name)
  (insert-and-indent (concat "def " method-name))
  (newline)
  (yank)
  (insert-and-indent "end")
  (newline))

(defun old-method-into-ripper()
  (beginning-of-defun 2)
  (comint-send-string (inf-ruby-proc) (concat "a = Ripper.sexp('" (get-method) "')\n"))
  (get-all-used))

(defun new-method-into-ripper()
  (beginning-of-defun)
  (comint-send-string (inf-ruby-proc) (concat "b = Ripper.sexp('" (get-method) "')\n"))
  (get-used))

(defun get-method() 
  ; need to escape any '
  (let (start)
    (setq start (point))
    (end-of-defun)
    (buffer-substring-no-properties start (point))))

(defun get-used()
  (comint-send-string (inf-ruby-proc) "(ast_refactor.find_used_assigned_vars b, [], results).join(', ')\n"))


(defun get-all-used()
  (comint-send-string (inf-ruby-proc) "results = ast_refactor.get_assigned_vars a, []\n"))

(defun insert-and-indent(text)
  (insert text)
  (indent-according-to-mode))

; find all local variables that are not defined
; check current method for definition, including passed in parameters
; if found pass in
; look for usage of variables that are no longer defined (were defined in the extracted code)
; inf-ruby
; require 'ripper'
; Ripper.sexp()

; run vote tally, look for assigns get idents "sum"
; look for var_field in boom that uses @ident sum if found pass it in
