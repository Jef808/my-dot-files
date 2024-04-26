;; Screen layouts
(defcommand hdmi-on () ()
 (exec "/home/jfa/.screenlayout/hdmi-on.sh"))

(defcommand hdmi-off () ()
  (exec "/home/jfa/.screenlayout/hdmi-off.sh"))

(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot " window)))

(defcommand set-keyboard-settings () ()
  (run-shell-command "setxkbmap 'us,ca' -option grp:win_space_toggle"))

(defcommand prompt () ()
  (run-shell-command "prompt"))
