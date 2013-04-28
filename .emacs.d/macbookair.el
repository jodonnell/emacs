(set-frame-position (selected-frame) 0 20)
(set-frame-size (selected-frame) 203 56)

(setq visible-bell t)

(require 'tramp)
(setq tramp-default-method "scp")

(setq ispell-program-name "/usr/local/bin/aspell")
