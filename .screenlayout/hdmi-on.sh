#!/bin/sh

# Sets up the TV screen as external monitor
xrandr --output LVDS-1 --primary --mode 1280x720 --pos 237x1080 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal
