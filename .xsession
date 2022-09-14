#!/bin/zsh

xrdb -merge "$HOME/.Xresources"

# Swap the <Ctrl> and <Caps_Lock> keys
setxkbmap -option ctrl:swapcaps

# Add some fonts to the fonts path
xset +fp $HOME/.local/share/fonts/ttf

# et-book fonts for emacs' org-mode
xset +fp $HOME/.local/share/fonts/et-book

# To avoid loading the accessibility features
export NO_AT_BRIDGE=1

xsetroot -cursor_name left_ptr
xset s off
xset b off

# Symlink to /usr/lib/ssh/x11-ssh-askpass
# export SSH_ASKPASS=ssh-askpass

#/usr/bin/redshift &

sbcl --load $HOME/.stumpwmstarter
