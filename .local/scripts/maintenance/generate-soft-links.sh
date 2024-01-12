#!/usr/bin/env sh

find /home/jfa/.local/scripts -type f -perm /u+x -print0 | xargs -0 -n1 -i{} bash -c 'echo -n'
