;; Manages the backlight intensity and gamma factor in terms of of the
;; current location and time of the day.
(run-shell-command "redshift")

;; Manages the notifications received via dbus
(run-shell-command "dunst")

;; Set up a background picture on any root page.
(run-shell-command "feh --bg-scale ~/.local/share/backgrounds/arch-linux.jpg")
