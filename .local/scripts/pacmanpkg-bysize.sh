#!/usr/bin/env sh

# Display the installed packages managed by pacman, sorted
# in reverse size order.
expac "%n %m" -l'\n' -Q -H M $(pacman -Qq) | sort -rhk 2 | bat
