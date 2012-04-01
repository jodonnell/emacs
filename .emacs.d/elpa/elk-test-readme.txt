;;; Commentary:
;;
;; For best visualization, install fringe-helper, which is available at:
;; http://nschum.de/src/emacs/fringe-helper/
;;
;; Use `deftest' to define a test and `elk-test-group' to define test groups.
;; `elk-test-run' can run tests by name, and `elk-test-run-buffer' runs them by
;; buffer.
;;
;; Tests can be defined anywhere, but dedicated (.elk) test files are
;; encouraged.  A major mode for these can be enabled like this:
;;
;; (add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
;;
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code like this:
;;
;; (deftest "test 1"
;;   (assert-eql 5 (+ 2 3)))
;;
;; (deftest "test 2"
;;   (assert-equal '(x y) (list 'x 'y))
;;   (assert-eq 'x (car '(x y))))
;;
;; (deftest "test 3"
;;   (assert-equal '(x y) (list 'y 'x))) ;; this will fail
;;
;; You can then run every test in the current buffer with `elk-test-run-buffer',
;; in a different buffer with `elk-test-run-a-buffer', or individual tests and
;; test groups with `elk-test-run'.
;;
;; To jump to failures, use `next-error', or click on the links in the error
;; buffer.  The optional `elk-test-result-follow-mode' will automatically
;; display the corresponding failure location.  To enable it by default, use the
;; following configuration:
;;
;; (add-hook 'elk-test-result-mode-hook 'elk-test-result-follow-mode)
;;
;;
;; To bind some keys, add the following to your .emacs:
;;
;; (define-key elk-test-mode-map (kbd "M-<f7>") 'elk-test-run-buffer)
;; (define-key emacs-lisp-mode-map (kbd "<f7>") 'elk-test-run-a-buffer)
;;
;;
;; To create your own assertions, use `assert-that'.  For example, the following
;; code defines `assert-eq' using `assert-that':
;;
;; (defmacro assert-eq (expected actual)
;;   "Assert that ACTUAL equals EXPECTED, or signal a warning."
;;   `(assert-that (lambda (actual) (eq ,expected ,actual))
;;                 actual
;;                 "assert-eq"
;;                 (lambda (actual)
;;                   (format "expected <%s>, was <%s>" ,expected ,actual))))
;;
;;
