#!/usr/bin/env zsh

# Ensure a man page exists
cmd_description() {
  manpage=$(man -k . | --query="$1" --select-1 --exit-0 | awk '{print $1}')
  if [ -z $manpage ]; then
      echo "Man page not found."
      exit 2
  fi

  # NAME section and DESCRIPTION section
  man "${manpage}" | col -bx | awk '
/^NAME/,/^$/{
  if (!seen) {print; if (/^$/) seen=1}
}
/^DESCRIPTION/,/^$/{
  if (!desc && /^DESCRIPTION) desc=1;
  else if (desc && /^$/) exit;
  else if (desc) print
}'
}
