;;; rinari-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rinari-minor-mode rinari-launch) "rinari" "../../../../.emacs.d/elpa/rinari-2.9/rinari.el"
;;;;;;  "9504c98c73d3b3ced717a22e309f4716")
;;; Generated autoloads from ../../../../.emacs.d/elpa/rinari-2.9/rinari.el

(autoload 'rinari-launch "rinari" "\
Call function `rinari-minor-mode' if inside a rails project.
Otherwise, disable that minor mode if currently enabled.

\(fn)" t nil)

(defvar rinari-major-modes (if (boundp 'rinari-major-modes) rinari-major-modes (list 'find-file-hook 'mumamo-after-change-major-mode-hook 'dired-mode-hook)) "\
Major Modes from which to launch Rinari.")

(dolist (hook rinari-major-modes) (add-hook hook 'rinari-launch))

(autoload 'rinari-minor-mode "rinari" "\
Enable Rinari minor mode to support working with the Ruby on Rails framework.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/rinari-2.9/rinari-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/rinari-2.9/rinari.el") (20570
;;;;;;  29686 690721))

;;;***

(provide 'rinari-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rinari-autoloads.el ends here
