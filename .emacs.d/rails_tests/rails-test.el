(defun find-rails-root-from-test-filename (filename)
  ()
)

; 
(defun get-parent-directory (filename)
  (if (path-end-with-a-file? filename)
      ()
    (string-match "/.*/.*\.rb$")
    
)

(defun path-end-with-a-file?(filename)
  (if (string-match "/.*/.*\.rb$" filename)
      t 
    nil))

(defun find-last-/ (path)
  
  )