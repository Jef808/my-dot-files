;; Manages the backlight intensity and gamma factor in terms of of the
;; current location and time of the day.
(run-shell-command "systemctl --user start redshift.service")

;; Notification daemon
(run-shell-command "systemctl --user start dunst")

;; Set up a background picture on any root page.
(run-shell-command "feh --bg-scale ~/.local/share/backgrounds/arch-linux.png")
