;; -*-lisp-*-/
;;
;;; stumpwm config

(require :stumpwm)
(in-package :stumpwm)

(defvar *config-dir* "/home/jfa/.config/stumpwm.d/"
  "The directory where the stumpwm configuration files are stored.")
(init-load-path "/home/jfa/.config/stumpwm.d/stumpwm-contrib/")
(setf *data-dir* "/home/jfa/.local/share/stumpwm/")

(load (concatenate 'string *config-dir* "xinit.lisp"))
(load (concatenate 'string *config-dir* "init-daemons.lisp"))
(load (concatenate 'string *config-dir* "commands.lisp"))
(load (concatenate 'string *config-dir* "keybindings.lisp"))
(load (concatenate 'string *config-dir* "modeline.lisp"))

(load-module "pass")

(load-module "stump-volume-control")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

;;(load-module "pinentry")

;; emacs everywhere
;(defcommand emacs-everywhere () ()
;  (run-shell-command "emacsclient --eval '(emacs-everywhere)'"))
;(define-key *root-map* (kbd "E") "emacs-everywhere")

;; Message window font
;; NOTE: Doesn't use fontconfig, need to make sure Xorg knows the font
;; can do that with mkfontdir <fontpath> && xset +fp <fontpath>, then
;; get the available XLFD (X Logical Font Description) string with xlsfonts
;; or xfontsel
;(ql:quickload :clx-truetype)
;(load-module "ttf-fonts")
;(set-font "-misc-jetbrains mono-medium-r-normal--0-0-0-0-m-0-*")
(set-font "-misc-fira mono-normal-*-*-*-*-*-*-*-*-*-*")
;(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")

;; Focus whatever window you click
(setf *mouse-focus-policy* :click)

;; Input and Message window placements
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top)
(setf *input-window-gravity* :center)

(enable-mode-line-on-all-screens)

;; (unless (head-mode-line (curent-head))
;;   (toggle-mode-line (current-screen) (current-head)))

;; Load Slynk
(ql:quickload :slynk)
(slynk:create-server :dont-close t)



;;;;;;;;;;;;;;;;;;;;;
;; Profiling       ;;
;;;;;;;;;;;;;;;;;;;;;

;(load "/home/jfa/.config/stumpwm.d/profiling.lisp")
;(defcommand prof-start () ()
;  (sb-sprof:start-profiling))

;(defcommand prof-end () ()
;  (sb-sprof:stop-profiling)
;  (sb-sprof:report :sort-by :cumulative-time)
;  (sb-sprof:reset))

;;;;;;;;;;;;;;;;;;;
;; Rotating logs ;;
;;;;;;;;;;;;;;;;;;;
;; (require :log4cl)
;; (log4cl::config :file "/home/jfa/.config/stumpwm.d/log4cl-config.lisp")

;; (defun log-and-tee (logger string start end)
;;   (let ((log-string (subseq string start end)))
;;     (log4cl:info logger log-string)))

;; (defun tee-stdout ()
;;   (let ((stdout-logger (log4cl:get-logger "STDOUT"))
;;         (original-stdout sb-sys:*stdout*))
;;     (setf sb-sys:*stdout*
;;           (make-instance 'sb-gray:fundamental-output-stream
;;                          :output-fn (lambda (stream string start end)
;;                                       (log-and-tee stdout-logger string start end)
;;                                       (write-string string original-stdout :start start :end end))))))

;; (defun tee-stderr ()
;;   (let ((stderr-logger (log4cl:get-logger "STDERR"))
;;         (original-stderr sb-sys:*stderr*))
;;     (setf sb-sys:*stderr*
;;           (make-instance 'sb-gray:fundamental-output-stream
;;                          :output-fn (lambda (stream string start end)
;;                                       (log-and-tee stderr-logger string start end)
;;                                       (write-string string original-stderr :start start :end end))))))

;; (defun setup-logging ()
;;   (log4cl:config :file "/home/jfa/.config/stumpwm.d/log4cl-config.lisp")
;;   (tee-stdout)
;;   (tee-stderr)
;;   (log4cl:info logger "STDOUT and STDERR now redirected to log file."))

;; (setup-logging)
