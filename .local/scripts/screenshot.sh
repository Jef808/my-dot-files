#!/usr/bin/env sh

arg="$1"
win_name=$(xwininfo -tree -root | grep "${arg}" | awk -F\" '{print $2}')

xwd -name "${win_name}" -out ~/tmp/"${win_name}".xwd #| convert - ~/tmp/$win_name.jpg
