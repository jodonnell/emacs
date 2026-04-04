;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL CHANGES

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'cl-lib)

(load (expand-file-name "key-remaps.el" user-emacs-directory) nil 'nomessage)
(load (expand-file-name "colors.el" user-emacs-directory) nil 'nomessage)

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


(setq font-lock-maximum-decoration t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; cause el captain has a bug causing issues with bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
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

(setq-default indent-tabs-mode nil)

(load (expand-file-name "jacobs-functions.el" user-emacs-directory) nil 'nomessage)
(load (expand-file-name "ruby-hash-syntax.el" user-emacs-directory) nil 'nomessage)

(require 'midnight)

(setq calendar-latitude 40.74)
(setq calendar-longitude -74.01)

(setq calendar-location-name "New York, NY")

(if (equal (getenv "EMACS_ENV") "macbookair")
    (load "~/.emacs.d/macbookair.el"))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil)
  (let ((aspell "/opt/homebrew/bin/aspell"))
    (when (file-executable-p aspell)
      (setq ispell-program-name aspell))))

(setq column-number-mode t)

(setq w32-use-w32-font-dialog nil)

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(setq use-package-always-ensure t)

; this directory should be checked in
(require 'use-package)

(use-package keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(use-package clojure-mode)
(use-package coffee-mode)
(use-package php-mode)

(use-package deadgrep
  :init
  (global-set-key (kbd "\C-c\C-g") 'deadgrep))

(use-package lua-mode
  :config
  (add-hook 'lua-mode-hook (lambda()
                             (setq-local indent-tabs-mode nil)
                             (setq-local lua-indent-level 2)))
  :bind
  ("\C-i" . th-complete-or-indent)
  ("\C-c\C-t" . run-lua-tests))

(use-package rspec-mode)
(use-package haml-mode
  :hook (haml-mode . (lambda ()
                       (setq-local indent-tabs-mode nil))))

(use-package rainbow-mode)
(use-package sass-mode)

(use-package css-mode
  :hook (css-mode . (lambda ()
                      (rainbow-mode)
                      (yas-minor-mode 1)
                      (local-set-key "\C-i" 'th-complete-or-indent)
                      (setq-local css-indent-offset 4)
                      (setq-local indent-tabs-mode nil))))


(use-package flycheck)

;; enable typescript-tslint checker
(setq-default typescript-indent-level 2)

(use-package yaml-mode)

(use-package smart-mode-line)
(use-package smex
  :init
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key "\C-x\C-m" 'smex)
  (global-set-key "\C-xm"    'execute-extended-command)
  (global-set-key "\C-c\C-m" 'execute-extended-command))

(use-package flx-ido)
(use-package rvm)
(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-reload-all)

(use-package magit
  :init
  (global-set-key "\C-cg" 'magit-status)
  :config
  (add-hook 'magit-log-mode-hook (lambda()
                                   (local-set-key "\M-n" 'forward-word))))

(use-package helm)
(use-package projectile)
(use-package helm-projectile)
(use-package projectile-rails)

(use-package elixir-mode)
(use-package csv-mode)
(use-package iedit)
(use-package git-timemachine)

(use-package nameless)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-flake8-from-env ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "env"))
         (flake8 (and root
                      (expand-file-name "env/bin/flake8"
                                        root))))
    (when (and flake8 (file-executable-p flake8))
      (setq-local flycheck-python-flake8-executable flake8))))
(add-hook 'flycheck-mode-hook #'my/use-flake8-from-env)

;; (use-package js2-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("Jakefile$" . js2-jsx-mode))
;;   (add-to-list 'auto-mode-alist '("\\.es6$" . js2-jsx-mode))
;;   (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
;;   :config
;;   (add-hook 'js2-mode-hook (lambda()
;;                              (setq js-indent-level 4)
;;                              (setq sgml-basic-offset 4)
;;                              (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;;                              (setq js2-mode-show-parse-errors nil)
;;                              (setq js2-mode-show-strict-warnings nil)

(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))
(add-hook 'js-ts-mode-hook (lambda()
                             (setq-local js-indent-level 2)))

(use-package vue-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
  :config
  (add-hook 'vue-mode-hook (lambda()
                             (setq-local js-indent-level 2)
                             (setq-local sgml-basic-offset 2)
                             (yas-minor-mode 1)
                             (local-set-key "\C-i" 'th-complete-or-indent)
                             (setq-local indent-tabs-mode nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRIAL
(global-set-key (kbd "RET") 'newline-and-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENDTRIAL

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
          (lambda ()
            (setq-local comint-input-ring-size 100)
            (define-key shell-mode-map "\M-n" 'forward-word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUBY STUFF

(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

(add-hook 'ruby-mode-hook (lambda()
                            (local-set-key "\C-ca" 'get-rails-function-argument-list-at-point)
                            (local-set-key "\C-cd" 'get-rails-documentation)
                            (local-set-key "\C-cm" 'get-instance-methods-current)
                            (local-set-key "\C-cc" 'get-class-methods-current)
                            (local-set-key "\C-\M-p" 'ruby-beginning-of-block)
                            (local-set-key "\C-\M-n" 'ruby-end-of-block)
                            (local-set-key "\C-crs" 'rubymotion-spec)
                            (local-set-key "\C-crr" 'rubymotion-simulator)
                            (local-set-key "\C-crd" 'rubymotion-device)
                            (rspec-mode)
                            (local-set-key "\C-i" 'th-complete-or-indent)
                            (setq-local indent-tabs-mode nil)))

(add-hook 'projectile-mode-hook 'projectile-rails-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir mode
(add-hook 'elixir-mode-hook (lambda()
                              (local-set-key "\C-i" 'th-complete-or-indent)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HAML
(add-hook 'haml-mode-hook (lambda() ;
			    (setq-local indent-tabs-mode nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP STUFF
(add-hook 'emacs-lisp-mode-hook (lambda()
                                  (local-set-key "\C-i" 'th-complete-or-indent)
                                  (nameless-mode 1)
                                  (setq-local indent-tabs-mode nil)))


(defface elisp-function-face5
  '((t (:foreground "deepskyblue3")))
  "Face for simplified prefixes.")

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
    1 'elisp-function-face5)))

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
  (font-lock-flush))


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
                             (setq-local tab-width 4
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
   (flyspell-prog-mode))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;(add-hook 'find-file-hook 'flymake-find-file-hook)
;(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJ-C STUFF
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-hook 'objc-mode-hook (lambda()
			    (setq-local c-default-style "bsd"
                                            c-basic-offset 4
                                            indent-tabs-mode nil)
			    (local-set-key "\C-i" 'th-complete-or-indent)
	          (local-set-key "\C-c\C-o" 'ff-find-other-file)
	          (local-set-key "\C-c\C-a" 'create-header-for-method)
			    (flyspell-prog-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EWW
(add-hook 'eww-mode-hook (lambda()
			    (local-set-key "\M-n" 'next-word)))


(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO MODE
(require 'ido)
(ido-mode t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;;disable ido faces to see flx highlights.
(setq ido-use-faces nil)


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
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "lime green" "magenta3" "cyan3"
    "gray90"])
 '(custom-safe-themes
   '("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a"
     default))
 '(ido-max-prospects 18)
 '(jshint-configuration-path "/Users/jacobodonnell/programming/bubble_bobble/.jshintrc")
 '(package-selected-packages
   '(clojure-mode coffee-mode csv-mode deadgrep eat eglot elixir-mode
                  exec-path-from-shell flx-ido flycheck git-timemachine
                  haml-mode helm-projectile iedit keyfreq lua-mode magit
                  nameless php-mode projectile projectile-rails rainbow-mode
                  rspec-mode rvm sass-mode smart-mode-line smex use-package
                  vue-mode yaml-mode yasnippet))
 '(pretty-lambda-auto-modes
   '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode
               ruby-mode))
 '(rspec-use-rvm t)
 '(scss-compile-at-save nil)
 '(warning-suppress-types '(nil)))

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

(require 'helm-projectile)

(global-set-key "\C-x\C-f" 'helm-projectile)
(global-set-key "\C-xf" 'ido-find-file)

(require 'projectile)
(projectile-mode +1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


;(setq mac-option-modifier 'super) ; make opt key do Super


(setq ido-enable-flex-matching t)

(setq projectile-completion-system 'ido)
(setq gc-cons-threshold 20000000)

(require 'iedit)

(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)

(global-set-key "\C-c\C-g" 'deadgrep)
;(global-set-key "\M-." 'dumb-jump-go)


(when (display-graphic-p)
  (condition-case nil
      (set-frame-font "Menlo-20" nil t)
    (error nil)))



(fset 'convertDbToTypes
   [?\C-x ?\C-m ?r ?e ?p ?l ?a ?c ?e ?s ?t ?r ?i ?n ?g return ?D ?a ?t ?a ?T ?y ?p ?e ?s ?. ?I ?N ?T ?E ?G ?E ?R ?, return ?n ?u ?m ?b ?e ?r ?\; return ?\M-< ?\C-x ?\C-m ?r ?e ?p ?l ?a ?c ?e ?s ?t ?r ?i ?n ?g return ?D ?a ?t ?a ?T ?y ?p ?e ?s ?. ?S ?T ?R ?I ?N ?G ?, return ?s ?t ?r ?i ?n ?g ?\; return ?\M-< ?\C-x ?\C-m ?r ?e ?p ?l ?a ?c ?e ?s ?t ?r ?i ?n ?g return ?D ?a ?t ?a ?t ?y ?p ?e ?s ?. backspace backspace backspace backspace backspace backspace ?T ?y ?p ?e ?s ?. ?B ?O ?O ?L ?E ?A ?N ?. backspace ?, return ?b ?o ?o ?l ?e ?a ?n ?\; return ?\M-< ?\C-x ?\C-m ?r ?e ?p ?l ?a ?c ?e ?s ?t ?r ?i ?n ?g return ?D ?a ?t ?a ?T ?y ?p ?e ?s ?. ?D ?A ?T ?E ?, return ?t ?s backspace backspace ?m ?o ?m ?e ?n ?t ?. ?M ?o ?m ?e ?n ?t ?\; ?\C-h ?  ?| ?  ?n ?u ?l ?l return])

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (js-json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))


(use-package eglot
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  ;; Prefer vtsls for JS/TS/TSX
  (add-hook 'js-ts-mode-hook  (lambda () (local-set-key (kbd "M-.") #'my/jump-def)))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode)
                 . ("vtsls" "--stdio"))))


(defun my/--moved-p (buf pos)
  (or (not (eq (current-buffer) buf))
      (not (= (point) pos))))

(defun my/rg-jump-first-hit-in-file (sym)
  "Jump to the first ripgrep hit for SYM in the current file.
Returns non-nil if it jumped."
  (when (and sym (executable-find "rg") buffer-file-name)
    (let* ((pattern (format "\\b%s\\b" sym))
           (args    (list "--no-heading" "--column" "-n" "-P" "-S" pattern buffer-file-name))
           (out     (with-temp-buffer
                      (apply #'process-file "rg" nil t nil args)
                      (buffer-string))))
      ;; Expect: /abs/path/file.js:LINE:COLUMN:...
      (when (string-match (format "^%s:\\([0-9]+\\):\\([0-9]+\\):"
                                  (regexp-quote (expand-file-name buffer-file-name)))
                          out)
        (let ((ln  (string-to-number (match-string 1 out)))
              (col (string-to-number (match-string 2 out))))
          (goto-char (point-min))
          (forward-line (1- ln))
          (move-to-column (max 0 (1- col)))
          (recenter)
          t)))))

(defun my/jump-def ()
  "Try LSP/xref → dumb-jump → rg(current file) → project search."
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (start-buf (current-buffer))
         (start-pos (point)))
    ;; 1) LSP/xref
    (ignore-errors (call-interactively #'xref-find-definitions))
    (when (my/--moved-p start-buf start-pos) (cl-return-from my/jump-def))

    ;; 2) dumb-jump (force rg for reliability)
    (let ((dumb-jump-force-searcher 'rg))
      (ignore-errors (dumb-jump-go)))
    (when (my/--moved-p start-buf start-pos) (cl-return-from my/jump-def))

    ;; 3) ripgrep in current file (first hit)
    (when (and sym (my/rg-jump-first-hit-in-file sym))
      (cl-return-from my/jump-def))

    ;; 4) project-wide search as last resort
    (if sym
        (project-find-regexp (format "\\_<%s\\_>" (regexp-quote sym)))
      (call-interactively #'project-find-regexp))))

;; Bind so it wins over js/js-ts defaults
(add-hook 'js-ts-mode-hook (lambda () (local-set-key (kbd "M-.") #'my/jump-def)))
(add-hook 'js-mode-hook    (lambda () (local-set-key (kbd "M-.") #'my/jump-def)))
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)


(use-package eat
  :ensure t)
