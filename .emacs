;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL CHANGES
(setq load-path (append load-path (list "~/.emacs.d")))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq w32-use-w32-font-dialog nil)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(transient-mark-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq dabbrev-case-replace nil)
(setq kill-read-only-ok t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq backup-directory-alist '(("." . "~/.emacs.backups")))

(load "~/.emacs.d/jacobs-functions.el")

(require 'midnight)
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

(setq calendar-latitude 40.74)
(setq calendar-longitude -74.01)

(setq calendar-location-name "New York, NY")

(if (equal (getenv "EMACS_ENV") "mac_home")
    (load "~/.emacs.d/mac_home.el"))
(if (equal (getenv "EMACS_ENV") "ubuntu_work")
    (load "~/.emacs.d/ubuntu_work.el"))
(if (equal (getenv "EMACS_ENV") "macbookair")
    (load "~/.emacs.d/macbookair.el"))

(setq column-number-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELK
(require 'elk-test)
(add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
(define-key elk-test-mode-map (kbd "C-c C-t") 'elk-test-run-buffer)
(define-key emacs-lisp-mode-map (kbd "<f7>") 'elk-test-run-a-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ISEARCH SHIT
(add-hook 'isearch-mode-hook
	  (lambda ()
	    (define-key isearch-mode-map (kbd "C-h") 'backward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHELL MODE
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; Fix junk characters in shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt) ;; hide passwords
(add-hook 'shell-mode-hook
          '(lambda ()
             (setq history-length 100)
             (define-key shell-mode-map "\M-n" 'forward-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLORS
(set-background-color "black")
(set-foreground-color "ghost white")

(set-cursor-color  "purple")

(copy-face 'default  'font-lock-comment-face)
(set-face-foreground 'font-lock-comment-face "orange")

(copy-face 'default  'font-lock-variable-name-face)
(set-face-foreground 'font-lock-variable-name-face "color-147") ;; bluish, very pretty!

(copy-face 'default  'font-lock-function-name-face)
(make-face-bold 'font-lock-function-name-face nil 1)
(set-face-foreground 'font-lock-function-name-face "Coral") ;; does this even work?
(copy-face 'default  'font-lock-string-face)
;;(make-face-italic 'font-lock-string-face nil 1)
(set-face-foreground 'font-lock-string-face "green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS CUSTOM VARIABLES
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "magenta3"))))
 '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "royalblue1"))))
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "orange"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:foreground "green"))))
 '(font-lock-function-name-face ((((class color) (background dark)) (:foreground "Coral"))))
 '(font-lock-keyword-face ((((class color) (background dark)) (:foreground "aquamarine"))))
 '(font-lock-string-face ((((class color) (background dark)) (:foreground "chartreuse2"))))
 '(font-lock-type-face ((((class color) (background dark)) (:foreground "green"))))
 '(font-lock-variable-name-face ((((class color) (background dark)) (:foreground "green"))))
 '(underline ((t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT STUFF
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)

(add-hook 'js-mode-hook (lambda() 
 				  (local-set-key "\C-i" 'th-complete-or-indent)
				  (local-set-key "\C-c\C-t" 'js-run-tests)
 				  (setq indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUBY STUFF
(require 'rails-dream)
(require 'flymake-ruby)
(require 'rinari)
(require 'ruby-end)
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))


(add-hook 'ruby-mode-hook (lambda() 
                            (global-set-key "\C-ca" 'get-rails-function-argument-list-at-point)
                            (global-set-key "\C-cd" 'get-rails-documentation)
                            (global-set-key "\C-cm" 'get-instance-methods-current)
                            (global-set-key "\C-cc" 'get-class-methods-current)
			    (local-set-key "\C-i" 'th-complete-or-indent)
                            (flymake-ruby-load)
                            (setq rinari-tags-file-name "TAGS")

			    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HAML
(require 'haml-mode)
(add-hook 'haml-mode-hook (lambda() 
			    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COFFEESCRIPT

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook (lambda() 
                            (set (make-local-variable 'tab-width) 2)
                            (local-set-key "\C-i" 'th-complete-or-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP STUFF
(add-hook 'emacs-lisp-mode-hook (lambda() 
			    (local-set-key "\C-i" 'th-complete-or-indent)
			    (setq indent-tabs-mode nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERL STUFF
(autoload 'cperl-mode "cperl-mode" "" t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-hairy t) ;; Turns on most of the CPerlMode options

;; makes cperl mode get activated when you open a perl file
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llmh]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; add hook to cperl mode
(add-hook 'cperl-mode-hook (lambda() 
                             (setq tab-width 4
                                   cperl-indent-level 4
                                   indent-tabs-mode nil
                                   cperl-invalid-face nil) ;; turns off show trailing whitespace
			     (show-paren-mode t)
			     (hs-minor-mode t)
			     (flyspell-prog-mode)
			     (local-set-key "\C-i" 'th-complete-or-indent)
			     (cperl-define-key "\C-j" 'universal-argument)
			     (cperl-define-key "\C-h" nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON STUFF
(defun my-python-mode-hook ()
   (local-set-key "\C-i" 'th-complete-or-indent)
   (show-paren-mode 1)
   (local-set-key "\C-c\C-t" 'py-run-tests)
   (flyspell-prog-mode))
(add-hook 'python-mode-hook 'my-python-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJ-C STUFF
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-hook 'objc-mode-hook (lambda()
			    (setq c-default-style "bsd"
				  c-basic-offset 4
				  indent-tabs-mode nil)
			    (local-set-key "\C-i" 'th-complete-or-indent)
			    (flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBFUFER MODE
(defun sw-list ()
  "switch to Ibuffer; this function should be bound to F9"
  (interactive)
  (setq buffer (get-buffer "*Ibuffer*"))
  (if (bufferp buffer)
      (progn 
        (switch-to-buffer buffer)
        (ibuffer-update nil))
    (ibuffer))
  (delete-other-windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL KEY MAPPINGS
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-c\C-y" 'transpose-chars)
(global-set-key "\C-t"     'dabbrev-expand)
(global-set-key "\C-z"     'advertised-undo)
(global-set-key "\C-ch"    'help-command)
(global-set-key "\M-s"     'isearch-forward-regexp)
(global-set-key "\M-g"     'goto-line)
(global-set-key "\C-xt"    'toggle-truncate-lines)
(global-set-key "\C-xa"    'find-file-at-point)
(global-set-key "\C-xp"    'shell)
(global-set-key "\C-x\C-p" 'rename-buffer)
(global-set-key [f8]       'sw-list)
(global-set-key "\C-cp"    'new-shell)
(global-set-key "\C-x\C-y" 'yank-regexp)

(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-b ?\C-c)
(keyboard-translate ?\C-c ?\C-b)
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'forward-char)

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
(global-set-key (kbd "C-o") 'dabbrev-expand)

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'universal-argument)

(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'universal-argument)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO MODE
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(setq org-hide-leading-stars 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAVEPLACE
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITIONS


(defun debugging-text ()
  "Inserts debug text which is language dependant"
  (interactive)
  (setq current-file-extension (file-name-extension (buffer-file-name)))
  (if (or (string= "pl" current-file-extension) (string= "pm" current-file-extension))
      (progn (insert-string  "use Data::Dumper; warn Dumper();")
	     (backward-char 2)))
  (if (string= "py" current-file-extension) 
      (insert-string "import pdb; pdb.set_trace()"))
  (if (string= "js" current-file-extension) 
      (progn (insert-string "console.log();")
	     (backward-char 2))))

(global-set-key "\C-x\C-t" 'debugging-text)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pretty-lambda-auto-modes (quote (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode ruby-mode)))
 '(warning-suppress-types (quote (nil))))


(defun pass-buffer-to-racket ()
  (interactive)
  (save-excursion
    (shell-command-on-region (beginning-of-buffer) (end-of-buffer) "racket")))

(add-hook 'scheme-mode-hook (lambda() 
                             (setq tab-width 4
                                   indent-tabs-mode nil)
			     (show-paren-mode t)
			     (hs-minor-mode t)
			     (flyspell-prog-mode)
			     (local-set-key "\C-i" 'th-complete-or-indent)
			     (local-set-key "\C-x\C-e" 'pass-buffer-to-racket)))




(defun js-run-tests ()
  (interactive)

  (shell "js-test")

  (erase-buffer)
  (shell-insert-send-sleep "java -jar JsTestDriver-1.3.0.jar --verbose --tests all --captureConsole --server http://localhost:9999" 1))
  
(defun py-run-tests ()
  (interactive)

  (shell "py-test")

  (erase-buffer)
  (shell-insert-send-sleep "python manage.py test villagevines" 1))
  
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)


(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))



(require 'clojure-mode)
(require 'magit)
(global-set-key "\C-cg" 'magit-status)
(require 'php-mode)
(require 'yaml-mode)

(require 'yasnippet-bundle)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMEX MODE
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xm"    'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

