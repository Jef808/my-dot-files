#!/usr/bin/env sh

# Check if the correct number of arguments is provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 input.pdf output_directory"
    exit 1
fi

# Assign input arguments to variables
input_pdf=$1
output_dir=$2

# Check if the input file exists
if [ ! -f "$input_pdf" ]; then
    echo "Input file not found!"
    exit 1
fi

# Check if the output directory exists, create if it doesn't
if [ ! -d "$output_dir" ]; then
    mkdir -p "$output_dir"
fi

# Extract the base name of the input PDF without the extension
base_name=$(basename "$input_pdf" .pdf)

# Convert PDF to JPEG with the same base name
magick -density 300 "$input_pdf" "$output_dir/${base_name}.jpg"

# Notify the user of completion
echo "Conversion complete. JPEG file is located in $output_dir/${base_name}.jpg"
