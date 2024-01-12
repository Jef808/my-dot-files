#!/usr/bin/env sh

xwd -out "$HOME/ss.xwd"

while ! [ -f "$HOME/ss.xwd" ]; do
    sleep 1
done;

convert "$HOME/ss.xwd" "$HOME/ss.png"
rm "$HOME/ss.xwd"
