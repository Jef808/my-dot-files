#!/usr/bin/env sh

# abort on errors
set -e

active_window_id=$(xdotool getactivewindow)

xdotool key --window "$active_window_id" ctrl+l
xdotool key --window "$active_window_id" alt+w

sleep 0.5

url=$(xclip -o -selection clipboard)

echo "$url"
