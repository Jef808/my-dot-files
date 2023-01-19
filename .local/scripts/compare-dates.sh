/usr/bin/env sh

date1=$1
date2=$2

if [[ "$date1" > "$cond" ]];
then
    echo "break"
fi
