;; Swap the <Ctrl> and <Caps_Lock> keys
;; and enable Quebec kbd layout... switch layouts
;; with "s-SPC" as keybinding for switching between the two layouts.
(run-shell-command "setxkbmap 'us,ca' -option ctrl:swapcaps -option grp:win_space_toggle")

;; To avoid loading the accessibility features
(setenv "NO_AT_BRIDGE" 1)

;xsetroot -cursor_name left_ptr

;; Disable screen saver
(run-shell-command "xset s off")

;; Disable sound bell
(run-shell-command "xset b off")
