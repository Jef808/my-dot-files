#!/bin/zsh

# Function to get the short description of a command
get_command_description() {
    local input_string="$1"
    local command_name=$(echo $input_string | awk '{print $1}') # Extracting the command name
    local args=$(echo $input_string | cut -d ' ' -f 2-) # Extracting the arguments

    # Fetching command description
    local cmd_desc=$(man $command_name | col -bx | awk '/^NAME/,/^SYNOPSIS/{ if ($1 != "NAME" && $1 != "SYNOPSIS") print; else if ($1 == "SYNOPSIS") exit }')

    # Checking if arguments are present
    if [[ -n $args ]]; then
        # Attempt to fetch help for each argument
        for arg in $args; do
            local arg_desc=$( $command_name --help 2>&1 | grep -E -o "$arg[^,]*" | head -n 1 )
            echo "Description of '$command_name $arg': $arg_desc"
        done
    else
        echo "Description of '$command_name': $cmd_desc"
    fi
}
