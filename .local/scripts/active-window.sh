#!/bin/env zsh

# Get the ID of the currently active window
active_window_id=$(xprop -root _NET_ACTIVE_WINDOW | awk '{print $5}')

# If no active window ID is found, exit the script
if [[ -z "$active_window_id" ]]; then
  echo "No active window detected."
  exit 1
fi

# Get the class name of the active window using its ID
window_class_name=$(xprop -id $active_window_id WM_CLASS | awk -F "\"" '{print $4}')

# Check if we successfully retrieved a class name
if [[ -z "$window_class_name" ]]; then
  echo "Could not retrieve the class name of the active window."
  exit 1
else
  echo "The class name of the active window is: $window_class_name"
fi
