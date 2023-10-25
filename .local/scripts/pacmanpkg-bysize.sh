#!/usr/bin/env sh

# Display the installed packages managed by pacman, sorted
# in reverse size order.

expac "%n %m" -Q -H M | sort -rhk 2 | awk '{printf "%-32s    %s %s\n", $1, $2, $3}'
