(require :stumpwm)
(in-package :stumpwm)

;; Modeline formatting
(setf *mode-line-pad-x* 5
      *mode-line-pad-y* 8)
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format*
      "[^B%n^b] %v^>%d   ")
(setf *mode-line-border-width* 3)
