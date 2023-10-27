#!/bin/bash

# Handle the ssh directory
if [ ! -d "$HOME/.ssh" ]; then
   mkdir "$HOME/.ssh"
fi
chown -R $(whoami) ~/.ssh/
find $HOME/.ssh       -type d -exec chmod 700 {} +
find $HOME/.ssh/*     -type f -exec chmod 600 {} +
find $HOME/.ssh/*.pub -type f -exec chmod 644 {} +

# Handle the gnupg directory
if [ ! -d "$HOME/.gnupg" ]; then
   mkdir "$HOME/.gnupg"
fi
chown -R $(whoami) ~/.gnupg/
find $HOME/.gnupg -type d -exec chmod 700 {} +
find $HOME/.gnupg -type f -exec chmod 600 {} +
