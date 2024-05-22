#!/bin/bash

#du -bs "$@"
function get_size {
    OIFS=$IFS
    IFS=$'\n'
    sum=0
    for sl in $(du -bs "$@"); do
        IFS=$'\t'
        strarr=($sl)
        sum=$(($sum + ${strarr[0]}))
    done
    IFS=$OIFS
    echo $sum
}
total_size=$(get_size "$@")
IFS=$'\n'
for line in $(df | tail -n +2) ; do
    IFS=$' '
    strarr=($line)
    avail="${strarr[3]}"
    if [[ $total_size -lt $avail ]]; then
        echo "${strarr[0]} ${strarr[5]}"
    fi
done | column -t
