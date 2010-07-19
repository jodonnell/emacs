
(set-frame-size (selected-frame) 271 67)
(set-frame-position (selected-frame) 0 20)

(setq mac-option-modifier 'meta)
(setq visible-bell t)

(require 'tramp)
(setq tramp-default-method "scp")
;;/multi:ssh:jodonnell@devt1.cheetahmail.com:ssh:jodonnell@p-devv1.sgc:/home/jodonnell/cm_develop/


;; (require 'imenu)
;; (defun ido-goto-symbol ()
;;   "Will update the imenu index and then use ido to select a symbol to navigate to"
;;   (interactive)
;;   (imenu--make-index-alist)
;;   (let ((name-and-pos '())
;; 	(symbol-names '()))
;;     (flet ((addsymbols (symbol-list)
;; 		       (when (listp symbol-list)
;; 			 (dolist (symbol symbol-list)
;; 			   (let ((name nil) (position nil))
;; 			     (cond
;; 			      ((and (listp symbol) (imenu--subalist-p symbol))
;; 			       (addsymbols symbol))
;; 			      ((listp symbol)
;; 			       (setq name (car symbol))
;; 			       (setq position (cdr symbol)))
;; 			      ((stringp symbol)
;; 			       (setq name symbol)
;; 			       (setq position (get-text-property 1 'org-imenu-marker symbol))))
;; 			     (unless (or (null position) (null name))
;; 			       (add-to-list 'symbol-names name)
;; 			       (add-to-list 'name-and-pos (cons name position))))))))
;;       (addsymbols imenu--index-alist))
;;     (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
;; 	   (position (cdr (assoc selected-symbol name-and-pos))))
;;       (if (markerp position)
;; 	  (goto-char position) (goto-char (overlay-start position))))))

;; (setq imenu-auto-rescan t)
