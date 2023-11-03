;; Screen layouts
(defcommand hdmi-on () ()
  (run-shell-command "hdmi-on"))
(defcommand hdmi-off () ()
  (run-shell-command "hdmi-off"))
(defcommand screenshot (window)
  ((:string "Enter the window name: "))
  (run-shell-command (concat "screenshot " window)))
