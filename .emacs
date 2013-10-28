;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq make-backup-files nil)
(setq visible-bell t)

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
(add-to-list 'pretty-lambda-auto-modes 'javascript-mode)
(pretty-lambda-for-modes)

(setq calendar-latitude 40.74)
(setq calendar-longitude -74.01)

(setq calendar-location-name "New York, NY")

(if (equal (getenv "EMACS_ENV") "macbookair")
    (load "~/.emacs.d/macbookair.el"))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH") ":/usr/local/git/bin:/usr/local/mysql-5.5.14-osx10.6-x86_64/bin:~/bin"))
  (setq ispell-program-name "/usr/local/bin/ispell")
  (push "/usr/local/git/bin" exec-path))

(setq column-number-mode t)



(add-hook 'log-edit-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-n"))
            (local-set-key (kbd "M-n") 'forward-word)))


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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; JAVASCRIPT STUFF
;;(require 'flymake-jshint)
;; (push "/Users/jacobodonnell/programming/bubble_bobble/node_modules/jshint/bin" exec-path)
;; (setenv "PATH" (concat "/Users/jacobodonnell/programming/bubble_bobble/node_modules/jshint/bin:" (getenv "PATH")))

(add-hook 'js-mode-hook (lambda() 
 				  (local-set-key "\C-i" 'th-complete-or-indent)
				  (local-set-key "\C-c\C-t" 'js-run-tests)
;;                                  'flymake-mode
 				  (setq indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUBY STUFF
(require 'rails-dream)
(require 'flymake-ruby)
(require 'rinari)
;(require 'show-args)
(require 'rspec-mode)
(require 'rvm)
;;(require 'robe)
(require 'company)
(push 'company-robe company-backends)

(setq load-path (append load-path (list "~/.emacs.d/rhtml")))
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook (lambda() 
                             (setq indent-tabs-mode nil)))


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
                            
                            (global-set-key "\C-crs" 'rubymotion-spec)
                            (global-set-key "\C-crr" 'rubymotion-simulator)
                            (global-set-key "\C-crd" 'rubymotion-device)
                            (rspec-mode)
;                            (robe-mode)
                            (company-mode)
			    (local-set-key "\C-i" 'th-complete-or-indent)
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
;; LUA STUFF
(add-hook 'lua-mode-hook (lambda()
                           (setq indent-tabs-mode nil)
                           (setq lua-indent-level 2)
                           (local-set-key "\C-i" 'th-complete-or-indent)
                           (local-set-key "\C-c\C-t" 'run-lua-tests)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJ-C STUFF
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-hook 'objc-mode-hook (lambda()
			    (setq c-default-style "bsd"
				  c-basic-offset 8
				  indent-tabs-mode t)
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
;; HIGHLIGHT SYMBOL AT POINT
;(global-set-key (kbd "<return>") 'newline) 
(require 'highlight-symbol)
(global-set-key [(control .)] 'highlight-symbol-at-point)
(global-set-key [(meta s)] 'highlight-symbol-next)
(global-set-key [(meta r)] 'highlight-symbol-prev)
(global-set-key [(meta m)] 'highlight-symbol-query-replace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL KEY MAPPINGS
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm"    'execute-extended-command)
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
(global-set-key "\C-xa"    'find-file-at-point-with-line)
(global-set-key "\C-xp"    'shell)
(global-set-key "\C-x\C-p" 'rename-buffer)
(global-set-key [f8]       'sw-list)
(global-set-key "\C-cp"    'new-shell)
(global-set-key "\C-x\C-y" 'yank-regexp)
(global-set-key "\C-cs"    'restart-server)

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

(global-set-key (kbd "C-c e") 'erase-buffer)


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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(jshint-configuration-path "/Users/jacobodonnell/programming/bubble_bobble/.jshintrc")
 '(pretty-lambda-auto-modes (quote (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode ruby-mode)))
 '(rspec-use-rvm t)
 '(scss-compile-at-save nil)
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
;;add melpa repository
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/"))


(require 'clojure-mode)
(require 'magit)
(global-set-key "\C-cg" 'magit-status)

(add-hook 'magit-log-mode-hook (lambda()
                                 (local-set-key "\M-n" 'forward-word)))


(require 'php-mode)
(require 'yaml-mode)

(require 'yasnippet-bundle)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))


(require 'sass-mode)
(require 'scss-mode)
(require 'rainbow-mode)
(add-hook 'scss-mode-hook (lambda()
                            (rainbow-mode)
                            (setq css-indent-offset 2
                                  indent-tabs-mode nil)))

(require 'rvm)

(require 'ctags)
(setq ctags-command "/usr/local/Cellar/ctags/5.8/bin/ctags -a -e -f TAGS --tag-relative -R app lib vendor")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMEX MODE
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xm"    'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;; thanks to steve yegge
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; ; -------------------- Autocomplete --------------------
;; ;; Use with Rsense for Ruby autocomplete:
;; ;; http://cx4a.org/software/rsense/
;; ;; Follow instructions on: http://itstickers.blogspot.com/2010/11/all-about-emacs.html
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

;; ;; Rsense
;; (setq rsense-home "/opt/rsense-0.3")
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)
 
;; ;; Rsense + Autocomplete
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))






; syp

(setq-default tab-width 2)

(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

(require 'projectile)
(projectile-global-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


(setq load-path (append load-path (list "~/.emacs.d/multi-web-mode/")))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'rhtml-mode)
(setq mweb-tags '((js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "erb"))
(multi-web-global-mode 1)
