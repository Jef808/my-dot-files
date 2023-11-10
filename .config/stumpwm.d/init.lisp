;; -*-lisp-*-/
;;
;;; stumpwm config

(require :stumpwm)
(in-package :stumpwm)

(defvar *config-dir* "/home/jfa/.config/stumpwm.d/")
(init-load-path "/opt/stumpwm-contrib/")
(setf *data-dir* "/home/jfa/.local/share/stumpwm/")

;; modules
;;(load-module "alert-me")
;(load-module "notify")
;(notify:notify-server-toggle)

(load "/home/jfa/.config/stumpwm.d/init-daemons.lisp")
(load "/home/jfa/.config/stumpwm.d/keybindings.lisp")
(load "/home/jfa/.config/stumpwm.d/commands.lisp")

(load-module "pinentry")

(run-shell-command "hdmi-on")

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
(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")

;; Input and Message window placements
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top)
(setf *input-window-gravity* :center)

;; Modeline formatting
(setf *mode-line-pad-x* 5
      *mode-line-pad-y* 8)
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format*
      "[^B%n^b] %v^>%d   ")
(setf *mode-line-border-width* 3)

;; Enable modeline
(enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) t)


;; Load Slynk
;(require :slynk)
;(require :uiop)
;(require :slynk)
;(defun run (command)
;  (sb-thread:make-thread
;   (lambda ()
;     (let ((exit-code (uiop:wait-process (uiop:launch-program command))))
;       (zerop exit-code)))
;   :name
;   (uiop:strcat "Waiting for '" command "'")))

;; (defun notify-send (msg &rest args)
;;   (run (format nil "notify-send ~{~a~^ ~}" (cons msg args))))

;; (defvar *slynk-default-port* 4005)
;; (defcommand slynk-start (port) ((:number "Port number: "))
;;   Start a slynk server manually from the parent sbcl process.
;;   Can then connect to it using `sly-connect' in emacs.
;;  (setq *slynkthread* (sb-thread:make-thread
;;                       (lambda ()
;;                          (slynk:create-server :port (or port *slynk-default-port*) :dont-close t)
;;                           )))
;;   nil)
;(defcommand slynk-stop () ((:string "Port number: "))
;  ())



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
