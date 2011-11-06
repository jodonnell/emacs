
(set-frame-position (selected-frame) 0 20)
(set-frame-size (selected-frame) 203 56)

(setq visible-bell t)

(require 'tramp)
(setq tramp-default-method "scp")
;;/multi:ssh:jodonnell@devt1.cheetahmail.com:ssh:jodonnell@p-devv1.sgc:/home/jodonnell/cm_develop/

(setq ispell-program-name "/usr/local/bin/aspell")
