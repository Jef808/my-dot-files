#!/usr/bin/env sh

xwd -out $HOME/ss.xwd

while true; do
    if [ -f "${HOME}/ss.xwd" ]; then
        convert $HOME/ss.xwd $HOME/ss.png
        rm $HOME/ss.xwd
        break
    else
        sleep 1
    fi
done
