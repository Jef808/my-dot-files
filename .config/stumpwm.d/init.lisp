;; -*-lisp-*-/
;;
;;; stumpwm config

(in-package :stumpwm)

;; modules
;;(load-module "alert-me")
;(load-module "notify")
;(notify:notify-server-toggle)

(defvar *config-dir* "/home/steve/.config/")

(setf *data-dir* "/home/steve/.local/share/stumpwm/")

(load "/home/steve/.config/stumpwm.d/keybindings.lisp")

;; Modeline formatting3
(setf *mode-line-pad-x* 5
      *mode-line-pad-y* 8)
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format*
      "[^B%n^b] %v^>%d   ")
(setf *mode-line-border-width* 3)

;; Message window font
;; NOTE: Doesn't use fontconfig, need to make sure Xorg knows the font
;; can do that with mkfontdir <fontpath> && xset +fp <fontpath>, then
;; get the available XLFD (X Logical Font Description) string with xlsfonts
;; or xfontsel
(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")

;; Input and Message window placements
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top)
(setf *input-window-gravity* :center)

;; Enable modeline
(enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) t)

;; Screen layouts
(defcommand hdmi-on () ()
  (run-shell-command "~/.screenlayout/hdmi-on"))
(defcommand hdmi-off () ()
  (run-shell-command "hdmi-off"))
(defcommand mishascreen-on () ()
  (run-shell-command "mishascreen-on"))
(defcommand mishascreen-off () ()
  (run-shell-command "mishascreen-off"))
(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot " window)))

;; emacs everywhere
(defcommand emacs-everywhere () ()
  (run-shell-command "emacsclient --eval '(emacs-everywhere)'"))
(define-key *root-map* (kbd "E") "emacs-everywhere")


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

(run-shell-command "dunst")
(run-shell-command "feh --bg-scale ~/.local/share/backgrounds/arch-linux.jpg")


;(defcommand slynk-stop () ((:string "Port number: "))
;  ())
