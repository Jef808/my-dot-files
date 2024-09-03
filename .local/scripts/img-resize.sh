#!/usr/bin/bash

# Function to display usage information
usage() {
    echo "Usage: $0 --size <WIDTH>x<HEIGHT> <input_image> <output_image>"
    exit 1
}

# Check if ImageMagick is installed
if ! command -v convert &> /dev/null; then
    echo "Error: ImageMagick is not installed. Please install it and try again."
    exit 1

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --size)
            if [[ $2 =~ ^[0-9]+x[0-9]+$ ]]; then
                SIZE=$2
                shift 2
            else
                echo "Error: Invalid size format. Use --size <WIDTH>x<HEIGHT>"
                usage
            fi
            ;;
        *)
            break
            ;;
    esac
done

# Check if size is provided
if [ -z "$SIZE" ]; then
    echo "Error: Size not specified."
    usage
fi

# Check if input and output image paths are provided
if [ $# -ne 2 ]; then
    echo "Error: Input and output image paths are required."
    usage
fi

INPUT_IMAGE=$1
OUTPUT_IMAGE=$2

# Check if input image exists
if [ ! -f "$INPUT_IMAGE" ]; then
    echo "Error: Input image does not exist."
    exit 1
fi

# Resize the image
magick "$INPUT_IMAGE" -resize "$SIZE" "$OUTPUT_IMAGE"

if [ $? -eq 0 ]; then
    echo "Image resized successfully: $OUTPUT_IMAGE"
else
    echo "Error: Failed to resize the image."
    exit 1
fi
