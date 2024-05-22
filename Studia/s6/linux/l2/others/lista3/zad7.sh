#!/bin/bash

IFS=$'\n'
du_res=$(du -sk $@)
sum_bytes=0
for i in $du_res; do
    IFS=$'\t'
    args=($i)
    sum=$(($sum + ${args[0]}))
done
echo $sum

IFS=$'\n'
for line in $(df | tail -n +2) ; do
    IFS=$' '
    args=($line)
    avail="${args[3]}"
    if [[ $sum -lt $avail ]]; then
        echo "${args[0]} ${args[5]}"
    fi
done | column -t