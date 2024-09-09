#!/usr/bin/env sh

# abort on errors
set -e

active_window_id=$(xdotool getactivewindow)

xdotool key --window "$active_window_id" ctrl+l
xdotool key --window "$active_window_id" alt+w
xdotool key --window "$active_window_id" Escape

# # with default vimium keybindings:
# xdotool key --window "$active_window_id" y
# xdotool key --window "$active_window_id" y

xclip -o -selection clipboard
