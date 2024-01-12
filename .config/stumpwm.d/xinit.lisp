;; Swap the <Ctrl> and <Caps_Lock> keys
;; and enable Quebec kbd layout, switch layouts with "s-SPC".
(run-shell-command "setxkbmap 'us,ca' -option ctrl:swapcaps -option grp:win_space_toggle")

;; Disable screen saver
(run-shell-command "xset s off")

;; Disable sound bell
(run-shell-command "xset b off")
