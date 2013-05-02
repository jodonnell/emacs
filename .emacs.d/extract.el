(defun extract-method(method-name)
  (interactive "sNew method name: ")
  (save-excursion 
    (replace-region-with-method method-name)
    (find-spot-to-insert-new-method)
    (insert-new-method method-name)
    (save-excursion
      (old-method-into-ripper))
    (new-method-into-ripper)
    (let (args)
      (setq args (grab-arguments))
      (replace-string method-name (concat method-name " " args)))))
    

(defun grab-arguments()
  (save-excursion
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
  (let (start old-method)
    (setq start (point))
    (end-of-defun)
    (setq old-method (buffer-substring-no-properties start (point)))
    (set-buffer "*ruby*")
    (insert "require 'ripper'")
    (comint-send-input)
    (insert "a = Ripper.sexp('")
    ; need to escape any '
    (insert old-method)
    (insert "')")
    (comint-send-input))
  (get-all-used))

(defun new-method-into-ripper()
  (beginning-of-defun)
  (let (start new-method)
    (setq start (point))
    (end-of-defun)
    (setq new-method (buffer-substring-no-properties start (point)))
    (set-buffer "*ruby*")
    (insert "b = Ripper.sexp('")
    ; need to escape any '
    (insert new-method)
    (insert "')")
    (comint-send-input))
  (get-used))

(defun get-used()
  (insert "def find_used_assigned_vars ruby_ast, var, assigned_vars;  next_is_var = false;  ruby_ast.each do |thing|;    return var.push(thing) if next_is_var and assigned_vars.include? thing;    if thing.is_a? Array;      find_used_assigned_vars thing, var, assigned_vars;    elsif thing == :@ident;      next_is_var = true;    end;  end;  return var;end; (find_used_assigned_vars b, [], results).join(', ')")
  (comint-send-input))

(defun get-all-used()
  (insert "def get_assigned_vars ruby_ast, var, assignment=false;  next_is_var = false;  ruby_ast.each do |thing|;    return var.push(thing) if next_is_var;    if thing.is_a? Array;      get_assigned_vars thing, var, assignment;    elsif thing == :assign;      assignment = true;    elsif thing == :@ident and assignment;      next_is_var = true;    end;  end;  return var;end;results = get_assigned_vars a, []")
  (comint-send-input))


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