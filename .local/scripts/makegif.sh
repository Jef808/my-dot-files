#!/usr/bin/env sh

ffmpeg \
    -i "$1" \
    -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" \
    -loop 0 \
    "$2"
