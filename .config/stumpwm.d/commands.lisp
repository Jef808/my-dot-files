;; Screen layouts
(defcommand hdmi-on () ()
  (run-shell-command "hdmi-on"))

(defcommand hdmi-off () ()
  (run-shell-command "hdmi-off"))

(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot " window)))

(defcommand set-keyboard-settings () ()
  (run-shell-command "setxkbmap 'us,ca' -option grp:win_space_toggle"))

(defcommand prompt () ()
  (run-shell-command "prompt"))
