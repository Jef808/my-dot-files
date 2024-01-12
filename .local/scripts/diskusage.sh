#! /usr/bin/sh
df -hT | awk '{ printf "%4s %6s %6s %16s\n",$5,$6,$4,$7 }'
