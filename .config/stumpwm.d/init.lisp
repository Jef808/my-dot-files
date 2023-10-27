;; -*-lisp-*-/
;;
;;; stumpwm config

(in-package :stumpwm)

(defvar *config-dir* "/home/steve/.config/stumpwm.d/")
(init-load-path "/usr/share/stumpwm/contrib/")
(setf *data-dir* "/home/steve/.local/share/stumpwm/")

;; modules
;;(load-module "alert-me")
;(load-module "notify")
;(notify:notify-server-toggle)

(load "/home/steve/.config/stumpwm.d/keybindings.lisp")
(run-shell-command "feh --bg-scale ~/.local/share/backgrounds/arch-linux.jpg")

(load-module "pinentry")

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
;(set-font "-xos4-terminus-bold-r-normal-*-18-*-*-*-*-*-*-*")
(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "JetBrains Mono" :subfamily "Regular" :size 14))

;; Input and Message window placements
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top)
(setf *input-window-gravity* :center)

;; Enable modeline

(enable-mode-line (stumpwm:current-screen) (stumpwm:current-head) t)

;; Screen layouts
(defcommand hdmi-on () ()
  (run-shell-command ~/.screenlayout/hdmi-on.sh))
(defcommand hdmi-off () ()
  (run-shell-command ~/.screenlayout/hdmi-off.sh))
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
;(defcommand slynk-stop () ((:string "Port number: "))
;  ())
