#!/usr/bin/sh

#List git repos in the current directory

find . -maxdepth 2 -mindepth 2 -name .git -exec dirname '{}' ';'
