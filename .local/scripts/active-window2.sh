#!/usr/bin/env zsh

# Ensure the .activewindow directory exists in the home directory
mkdir -p ~/.activewindow

# Use xprop to get the active window ID, then extract the WM_CLASS property
# which contains the class name. This is piped into awk to parse and print the
# second part (the actual class name). Then, write the output to the classname file.
xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) WM_CLASS | \
awk -F '"' '{print $(NF-1)}' > ~/.activewindow/classname

echo "The active window's class name has been written to ~/.activewindow/classname."
