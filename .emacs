;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL CHANGES
  (let ((default-directory "~/emacs"))
    (normal-top-level-add-to-load-path '("."))
   (normal-top-level-add-subdirs-to-load-path))

(setq w32-use-w32-font-dialog nil)

;(setq mac-command-modifier 'meta) ; make cmd key do Meta

;(setq mac-control-modifier 'control) ; make Control key do Control
;(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(require 'auto-cask)
(auto-cask/setup "~/emacs/")
(cask-initialize)


(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(transient-mark-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq visible-bell t)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)

;;(require 'exec-path-from-shell)
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

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
(load "~/.emacs.d/jacobs/django_tests.el")

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

(set-cursor-color  "firebrick1")

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

(add-to-list 'auto-mode-alist '("Jakefile" . js-mode))
(add-hook 'js-mode-hook (lambda() 
 				  (local-set-key "\C-i" 'th-complete-or-indent)
				  (local-set-key "\C-c\C-t" 'js-run-tests)
          ;(setq js-indent-level 2)
;;                                  'flymake-mode
 				  (setq indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUBY STUFF
;;(require 'rails-dream)
;;(require 'flymake-ruby)
;;(require 'rinari)
;(require 'show-args)
(require 'rspec-mode)
(require 'rvm)
;;(require 'robe)
;(require 'company)
;(push 'company-robe company-backends)

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

(defface elisp-prefix-face
  '((t (:foreground "grey50")))
  "Face for simplified prefixes.")

(defun elisp-simplify-prefix (prefix rep)
  "Replace PREFIX with REP visually on this buffer.

PREFIX is simply displayed as REP, but not actually replaced with REP."
  (interactive "sVisually replace this long prefix: \nsWith this short prefix: ")
  (font-lock-add-keywords
   nil `((
          ;; ;; not sure why these don't work
          ;; ,(rx-to-string `(group word-boundary ,prefix word-boundary))
          ;; ,(rx-to-string `(: word-boundary ,prefix word-boundary))
          ;; ,(rx-to-string `(: ,prefix))

          ,(rx-to-string `(group ,prefix))

          (0 (progn (put-text-property (match-beginning 0) (match-end 0)
                                       'display ,rep)
                    'elisp-prefix-face)))))
  (font-lock-fontify-buffer))


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
   (subword-mode 1)
   (local-set-key "\C-c\C-t" 'django-tests-run)
   (flyspell-prog-mode))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(when (load "flymake" t)
  (defun flymake-flake8-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8" (list "--max-line-length=120" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-flake8-init)))

;(add-hook 'find-file-hook 'flymake-find-file-hook)
;(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)


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
                                  c-basic-offset 4
                                  indent-tabs-mode nil)
			    (local-set-key "\C-i" 'th-complete-or-indent)
          (local-set-key "\C-c\C-o" 'ff-find-other-file)
          (local-set-key "\C-c\C-a" 'create-header-for-method)
			    (flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SWIFT STUFF
(add-hook 'swift-mode-hook (lambda()
			    (local-set-key "\C-i" 'th-complete-or-indent)))

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

(defun other-window-back()
  (interactive)
  (other-window -1))
(global-set-key "\C-x\C-o" 'other-window-back)

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


(global-set-key (kbd "s-a") 'erase-buffer)
(global-set-key (kbd "H-a") 'erase-buffer)
(global-set-key (kbd "A-a") 'erase-buffer)

(global-set-key (kbd "s-c") 'beginning-of-defun)
(global-set-key (kbd "s-t") 'end-of-defun)
(global-set-key (kbd "s-n") 'forward-sexp)
(global-set-key (kbd "s-h") 'backward-sexp)
(global-set-key (kbd "s-d") 'kill-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO MODE
(require 'ido)
(ido-mode t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(setq gc-cons-threshold 10000000) ; set emacs garbage collection to 10 mb


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)

(setq org-hide-leading-stars 0)
(defun my-org-mode-hook ()
   (local-set-key "\M-h" 'backward-word))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAVEPLACE
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED
(defun my-dired-mode-hook ()
   (local-set-key "\C-t" 'next-line))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(progn
  ;; modify dired keys
  (require 'wdired)
  (define-key wdired-mode-map (kbd "C-n") 'forward-char))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(debug-on-error t)
 '(ido-max-prospects 18)
 '(jshint-configuration-path "/Users/jacobodonnell/programming/bubble_bobble/.jshintrc")
 '(pretty-lambda-auto-modes
   (quote
    (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode ruby-mode)))
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
(add-to-list 'package-archives
              '("melpa" . "http://melpa.milkbox.net/packages/"))


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

;;(require 'ctags)
;;(setq ctags-command "/usr/local/Cellar/ctags/5.8/bin/ctags -a -e -f TAGS --tag-relative -R app lib vendor")
;;(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM
(require 'helm)

(define-key helm-map (kbd "C-b") 'helm-previous-line)
(define-key helm-map (kbd "C-t") 'helm-next-line)
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "s-f") 'helm-projectile)

(require 'helm)
(require 'helm-projectile)



; syp

(setq-default tab-width 2)

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

;; (require 'smart-mode-line)
;; (setq sml/theme 'dark)
;; (sml/setup)


(defun wrap-lines-region-html (b e tag)
  "'tag' every line in the region with a tag"
  (interactive "r\nMTag for line: ")
  (setq p (point-marker))
  (save-excursion
    (goto-char b)
    (while (< (point) p)
      (beginning-of-line)
      (indent-according-to-mode)
      (insert (format "<%s>" tag))
      (end-of-line)
      (insert (format "</%s>" tag))
      (forward-line 1))))

(defun insert-tag-at-position-and-indent (start format-tag)
  (goto-char start)
  (beginning-of-line)
  (indent-according-to-mode)
  (insert (format format-tag tag))
  (newline))

(defun wrap-region-html (b e tag)
  "'tag' every line in the region with a tag"
  (interactive "r\nMTag for line: ")
  (setq p (point-marker))
  (save-excursion
    (insert-tag-at-position-and-indent b "<%s>")
    (insert-tag-at-position-and-indent p "</%s>")
    (indent-region b p)
    (beginning-of-line)
    (indent-according-to-mode)))


(setq mac-option-modifier 'super) ; make opt key do Super


(defun rdio-play ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to playpause'"))

(defun rdio-next ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to next track'"))

(defun rdio-previous ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Rdio\" to previous track'"))

(global-set-key "\C-\M-n" 'rdio-next)
(global-set-key "\C-\M-h" 'rdio-previous)

(setq ido-enable-flex-matching t)

(setq projectile-completion-system 'ido)
(setq gc-cons-threshold 20000000)
