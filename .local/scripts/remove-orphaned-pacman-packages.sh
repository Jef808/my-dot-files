#!/usr/bin/sh env

pacman -Qdt | awk '{print $1}' | xargs -I {} sudo pacman -R --no-confirm {} 
