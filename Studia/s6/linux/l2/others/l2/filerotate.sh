#!/bin/bash


if [[ -z $1  || -z $2 ]]; then
    echo "USAGE: ./filerotate.sh <path> <size>"
    exit 1
fi
path=$1
sizeb=$2
total=$(du -bs | awk '{print $1}')

if [[ $sizeb -gt $total ]]; then
    echo "Provided size larger than total size (total=$total)"
    exit 1
fi

bytesToDelete=$(($total - $sizeb))
# find . -type f -exec stat -c '%W %s %n' {} + | sort -n
IFS=$'\n'
accsize=0
for line in $(find . -type f -exec stat -c $'%W\t%s\t%n' {} + 2>/dev/null | sort -n); do
    IFS=$'\t'
    strarray=($line)
    IFS=$'\n'
    if [[ $accsize -gt $bytesToDelete ]]; then
        break 1
    fi
    accsize=$(expr $accsize + ${strarray[1]})
    echo "${strarray[2]}"
done
