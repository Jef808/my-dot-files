#!/usr/bin/env bash

# Function to display usage information
usage() {
    echo "Usage: $0 [-s <scale>] [-o <output_directory>] <input_pdf>"
    echo "  -r <density>           The number of pixels per inch (optional, default: 108)"
    echo "  -o <output_directory>  Output directory (optional, default: current directory)"
    echo "  <input_pdf>            Input PDF file (required)"
    exit 1
}

# Default values
density=108
output_dir="."
input_pdf=""

# Parse command line arguments
while getopts ":s:o:" opt; do
    case $opt in
        s) density="$OPTARG" ;;
        o) output_dir="$OPTARG" ;;
        \?) echo "Invalid option -$OPTARG" >&2; usage ;;
        :) echo "Option -$OPTARG requires an argument"; usage ;;
    esac
done

# Shift to the non-option arguments
shift $((OPTIND-1))

# Check if input PDF is provided
if [[ $# -eq 0 ]]; then
    echo "Error: Input PDF file is required."
    usage
elif [[ $# -gt 1 ]]; then
    echo "Error: Too many arguments."
    usage
else
    input_pdf="$1"
fi

# Check if input PDF exists
if [[ ! -f "$input_pdf" ]]; then
    echo "Error: Input PDF file does not exist."
    exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "$output_dir"

# Convert PDF to PNG images
pdftocairo -png -r "$density" "$input_pdf" "$output_dir/page"

echo "Conversion complete. PNG images saved in $output_dir"
