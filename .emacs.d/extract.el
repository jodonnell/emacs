(defvar extract-process-name "extract-process")

(defvar extract-method-name nil)
(defvar extract-method-current-buffer nil)
(defvar extract-method-call-marker nil)

(defvar extract-ruby-path
  (let ((current (buffer-file-name)))
    (expand-file-name (file-name-directory current)))
  "Path to the backend Ruby code.")

(defun extract-method(method-name)
  (interactive "sNew method name: ")
  (setq extract-method-name method-name)
  (setq extract-method-current-buffer (current-buffer))
  (setup-process)
  (save-excursion 
    (move-code-to-new-method method-name)
    (add-arguments-to-method method-name)))

(defun add-arguments-to-method(method-name) ; bad method name
  (save-excursion
    (old-method-into-ripper))
  (new-method-into-ripper)
  (get-used))

(defun setup-process()
  (when (not (and (extract-process) (process-live-p (extract-process))))
    (progn
      (start-extract-process)
      (require-ruby-code))))

(defun start-extract-process()
  (let ((process-connection-type nil))  ; use a pipe
    (start-process extract-process-name nil "irb")
  (set-process-filter (extract-process) 'extract-process-filter)))
;(delete-process (extract-process))

(defun require-ruby-code()
  (process-send-string (extract-process)
                       (format (mapconcat #'identity
                                          '("unless defined? ASTRefactor"
                                            "$:.unshift '%s'"
                                            "require 'extract'"
                                            "require 'ripper'"
                                            "ast_refactor = ASTRefactor.new"
                                            "end\n")
                                          "; ")
                               extract-ruby-path)))

(defun extract-process-filter(process output)
  (set-buffer extract-method-current-buffer)
  (string-match "\"\\(.*\\)\"$" output)
  (let ((args (match-string 1 output)))
    (if args
        (progn
          (set-buffer extract-method-current-buffer)
          (beginning-of-buffer)
          (search-forward (concat "def " extract-method-name))
          (insert " ")
          (insert args)
          (goto-char extract-method-call-marker)
          (insert " ")
          (insert args)))))

(defun extract-process()
  (get-process extract-process-name))

(defun move-code-to-new-method(method-name)
  (replace-region-with-method method-name)
  (find-spot-to-insert-new-method)
  (insert-new-method method-name))

(defun replace-region-with-method(method-name)
  (kill-region (point) (mark))
  (insert-and-indent method-name)
  (setq extract-method-call-marker (point-marker))
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
  (beginning-of-defun)
  (beginning-of-defun)
  (process-send-string (extract-process) (concat "a = Ripper.sexp('" (get-method) "')\n")))

(defun new-method-into-ripper()
  (beginning-of-defun)
  (process-send-string (extract-process) (concat "b = Ripper.sexp('" (get-method) "')\n")))

(defun get-method() 
  ; need to escape any '
  (let (start)
    (setq start (point))
    (end-of-defun)
    (buffer-substring-no-properties start (point))))

(defun get-used()
  (process-send-string (extract-process) "((ast_refactor.get_variable_references b, []) & (ast_refactor.get_local_variables_from_caller a, [])).join(', ')\n"))

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


; look at new method.
; find all var_ref variables (these are all variables, methods etc used)
; look at old method 
; look at all var_field and params (these are all variables declared and passed in)
; the intersection needs to be passed in.
; also may need to add self.
; might need to return variabl
