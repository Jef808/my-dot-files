#!/usr/bin/env sh

# Check if a hostname was provided
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <hostname>"
  exit 1
fi

hostname=$1

# Use ping and arp
if [[ -z "$ip" ]]; then
  # Send a single ping to the hostname to attempt to populate the arp cache
  ping -c 1 "$hostname" &>/dev/null

  # Check the arp cache for the IP
  ip=$(arp -a | grep --fixed-strings "($hostname)" | grep -oP '(?<=\().*?(?=\))')
fi

# Print the IP if found
if [[ -n "$ip" ]]; then
  echo "$ip"
else
  exit 1
fi
