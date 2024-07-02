#!/usr/bin/env sh

# Function to generate a unique ID
generate_unique_id() {
    date +"%Y%m%d%H%M%S"
}

# Parse arguments for file name and output directory
FILE_NAME=${1:-ss}
OUTPUT_DIR=${2:-$HOME}

# Ensure the output directory exists
mkdir -p "$OUTPUT_DIR"

# Generate a unique file name
UNIQUE_ID=$(generate_unique_id)
XWD_FILE="$OUTPUT_DIR/${FILE_NAME}_$UNIQUE_ID.xwd"
PNG_FILE="$OUTPUT_DIR/${FILE_NAME}_$UNIQUE_ID.png"

# Take the screenshot and save it as .xwd
xwd -out "$XWD_FILE"

# Wait until the .xwd file is created
while ! [ -f "$XWD_FILE" ]; do
    sleep 1
done

# Convert the .xwd file to .png
magick "$XWD_FILE" "$PNG_FILE"

# Remove the .xwd file
rm "$XWD_FILE"

# Output the location of the saved screenshot
echo "Screenshot saved to $PNG_FILE"
