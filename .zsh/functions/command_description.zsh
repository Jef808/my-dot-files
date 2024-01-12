#!/usr/bin/env zsh

command_description() {
    local input_string=$1
    local command_name=$(echo $input_string | awk '{print $1}') # Extracting the command name
    local args=$(echo $input_string | cut -d ' ' -f 2-) # Extracting the arguments

    command_description=$(man $command_name | col -bx | awk '
/^[A-Z]/ {
    section = $0
    content = ""
    next
}
{
    content = content $0 "\n"
}
END {
    for (s in sections) {
        if(s ~ /^(NAME|DESCRIPTION)$/) {
            print sections[s]
        }
    }
}')
    print $command_description
}
