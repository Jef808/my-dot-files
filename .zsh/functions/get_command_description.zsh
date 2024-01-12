# Function to get the short description and the first paragraph of the DESCRIPTION section of a command
# get_command_description_old() {
#     local input_string="$1"
#     local command_name=$(echo $input_string | awk '{print $1}') # Extracting the command name
#     local args=$(echo $input_string | cut -d ' ' -f 2-) # Extracting the arguments

#     # Fetching command short description
#     local cmd_short_desc=$(man $command_name | col -bx | awk '/^NAME/,/^SYNOPSIS/{ if ($1 != "NAME" && $1 != "SYNOPSIS") print; else if ($1 == "SYNOPSIS") exit }' | xargs)

#     # Fetching first paragraph of the DESCRIPTION section
#     local cmd_desc=$(man $command_name | col -bx | awk '/^DESCRIPTION/,/^$/{ if ($1 != "DESCRIPTION" && $1 != "") print; else if ($1 == "") exit }' | awk '/./{line=$0} /^ /{print line; exit} !/^ /{if (NR!=1) print line; line=$0}')

#     echo "Command: $cmd_short_desc"
#     echo "Description: $cmd_desc"

#     # Checking if arguments are present
#     if [[ -n $args ]]; then
#         # Attempt to fetch help for each argument
#         for arg in $args; do
#             if [[ $arg == -* ]]; then
#                 # Fetching description of the argument
#                 local arg_desc=$( $command_name --help 2>&1 | awk -v arg="$arg" 'BEGIN{found=0} {if (match($0, "^\\s*" arg)) found=1} found{if (/^\\s*-/ && !match($0, "^\\s*" arg)) exit; else print}' | xargs)
#                 echo "Argument $arg: $arg_desc"
#             fi
#         done
#     fi
# }

# Function to get the short description and the first paragraph of the DESCRIPTION section of a command
get_command_description() {
    local input_string="$1"
    local command_name=$(echo $input_string | awk '{print $1}') # Extracting the command name
    local args=$(echo $input_string | cut -d ' ' -f 2-) # Extracting the arguments

    # Fetching command short description
    local cmd_short_desc=$(man $command_name | col -bx | awk '/^NAME/,/^SYNOPSIS/{ if ($1 != "NAME" && $1 != "SYNOPSIS") print; else if ($1 == "SYNOPSIS") exit }' | xargs)

    # Fetching first paragraph of the DESCRIPTION section
    local cmd_desc=$(man $command_name | col -bx | awk '/^DESCRIPTION/,/^$/{ if ($1 != "DESCRIPTION" && $1 != "") print; else if ($1 == "") exit }' | awk 'NR==1{printf $0; next} !/^ /{print ""; exit} {printf " " $0}' | xargs)

    echo "$command_name: $cmd_short_desc"
    echo "Description: $cmd_desc"

    # Checking if arguments are present
    if [[ -n $args ]]; then
        # Attempt to fetch help for each argument
        for arg in $args; do
            if [[ $arg == -* ]]; then
                # Fetching description of the argument
                local arg_desc=$( $command_name --help 2>&1 | awk -v arg="$arg" 'BEGIN{found=0} {if (match($0, "^\\s*" arg)) found=1} found{if (/^\\s*-/ && !match($0, "^\\s*" arg)) exit; else print}' | awk '{printf "%s ", $0}' | xargs)

                # If description not found, split the argument into individual characters and search again
                if [[ -z $arg_desc ]]; then
                    local individual_args=$(echo $arg | grep -o .)
                    for i in $individual_args; do
                        if [[ $i != "-" ]]; then
                            local single_arg_desc=$( $command_name --help 2>&1 | awk -v arg="-${i}" 'BEGIN{found=0} {if (match($0, "^\\s*" arg)) found=1} found{if (/^\\s*-/ && !match($0, "^\\s*" arg)) exit; else print}' | awk '{printf "%s ", $0}' | xargs)
                            echo "-$i: $single_arg_desc"
                        fi
                    done
                else
                    echo "$arg: $arg_desc"
                fi
            fi
        done
    fi
}
