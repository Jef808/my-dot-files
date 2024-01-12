#!/bin/bash

# Usage: sudo ./migrate_user.sh old_username new_username

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root"
   exit 1
fi

# Check for required arguments
if [ $# -ne 2 ]; then
    echo "Usage: sudo $0 old_username new_username"
    exit 1
fi

old_username="$1"
new_username="$2"

# Check for running processes by the old user
if pgrep -u $old_username; then
    echo "There are running processes for $old_username. Please ensure the user is not logged in and no user processes are running."
    exit 1
fi

# Step 1: Change the username
usermod -l $new_username $old_username

# Step 2: Move the home directory
mv /home/$old_username /home/$new_username

# Step 3: Update the user's home directory in the system
usermod -d /home/$new_username -m $new_username

# Step 4: Change ownership of all files owned by the old user
find /home/$new_username -user $old_username -exec chown $new_username:$new_username {} \; 2>/dev/null

echo "Migration completed successfully."
