#!/usr/bin/env sh

[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile
[ -f ~/.zprofile ] && . ~/.zprofile

xrdb -merge "$HOME/.Xresources"

# Add some fonts to the fonts path
xset +fp $HOME/.local/share/fonts/ttf

# et-book fonts for emacs' org-mode
xset +fp $HOME/.local/share/fonts/et-book

# To avoid loading the accessibility features
export NO_AT_BRIDGE=1

# Swap the <Ctrl> and <Caps_Lock> keys
setxkbmap -option ctrl:swapcaps

xsetroot -cursor_name left_ptr
xset s off
xset b off

# Notifications
# dunst &
# networkd-notify &

exec /opt/sbcl/bin/sbcl --load $HOME/.stumpwmstarter
