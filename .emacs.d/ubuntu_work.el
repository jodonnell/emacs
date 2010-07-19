;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP STUFF
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
	     '("\\." nil "/ssh:jodonnell@devt1:"))


(require 'magit)