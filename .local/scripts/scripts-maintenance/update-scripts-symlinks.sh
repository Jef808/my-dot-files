#!/bin/bash

handle_file() {
    local filepath=$(realpath "$1")
    local target_dir=$(realpath "$2")
    local filename=$(basename -- "$filepath")
    local targetname="${filename%.sh}"

    chmod +x "$filepath"

    ln -s "$filepath" "$target_dir/$targetname"
}

# Handling individual files
if [ "$1" == "handle-file" ]; then
    handle_file "$2" "$3"

# Handling directories
else
    if [ "$#" -ne 2 ]; then
        echo "Usage: $0 <source_directory> <target_directory>"
        exit 1
    fi

src_dir=$(realpath "$1")
target_dir=$(realpath "$2")

    if [ ! -d "$src_dir" ]; then
        echo "Error: Source directory '$src_dir' does not exist."
        exit 1
    fi

    if [ ! -d "$target_dir" ]; then
        mkdir -p "$target_dir"
    fi

    find "$src_dir" -type f -name "*.sh" -exec "$0" handle-file {} "$target_dir" \;
fi
