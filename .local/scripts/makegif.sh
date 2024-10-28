#!/usr/bin/env sh

# Check if resolution argument is provided
if [ -z "$2" ]; then
    resolution="320:-1"
else
    resolution="$2"
fi

if [ -z "$3" ]; then
    fps="$3"
else
    fps=10
fi

ffmpeg -i "$1" -vf "fps=$fps,scale=$resolution:flags=lanczos,palettegen" /tmp/palette.png
ffmpeg -i "$1" -i /tmp/palette.png -filter_complex "fps=$fps,scale=$resolution:flags=lanczos[x];[x][1:v]paletteuse" -loop 0 /tmp/output.gif
