#!/usr/bin/env sh

OUTPUT=$1

# Set default output file if not provided
if [ -z "$OUTPUT" ]; then
    OUTPUT="${HOME}/screenrecord.mp4"
fi

XRANDR_OUTPUT=$(xrandr)

# Get available screens and resolutions
SCREENS=$(echo "$XRANDR_OUTPUT" | grep " connected" | awk '{print $1}')

# Show available screens and prompt the user to select one
echo "Available screens:"
select SCREEN in $SCREENS; do
    if [ -n "$SCREEN" ]; then
        break
    else
        echo "Invalid selection. Please select a valid screen number."
    fi
done

SCREEN_SIZE=$(echo "$XRANDR_OUTPUT" | grep "^$SCREEN" | awk '{if ($3 == "primary") {print $4} else {print $3}}')

OFFSETS=$(echo "${SCREEN_SIZE#*+}" | sed 's/+/,/g')

RESOLUTION="${SCREEN_SIZE%%+*}"

# Verify if resolution was correctly extracted
if [ -z "$RESOLUTION" ] || [ -z "$OFFSETS" ]; then
    echo "Could not determine the resolution and/or offsets for $SCREEN."
    return 1
fi

echo "Recording screen $SCREEN..."

# Execute the ffmpeg command with dynamic resolution
ffmpeg \
    -f x11grab \
    -framerate 25 \
    -video_size "$RESOLUTION" \
    -i "${DISPLAY}+${OFFSETS}" \
    -codec:v libx264 \
    -preset ultrafast \
    -loglevel quiet \
    "$OUTPUT"
