#!/bin/bash

mount_point=$1

if [[ -z $mount_point ]]; then
    mount_point="."
fi 

result=$(grep -E "^UUID=[0-9a-zA-Z\-]+[ ]*$mount_point([/]*)[ ]+.*$" /etc/fstab)

args=($result)
if [[ ! -z $result ]]; then
    for i in $(seq 0 6 $(( ${#args[@]}-1 )) )
    do 
        echo "Device:               ${args[ $((0 + $i)) ]}"
        echo "Filesystem type:      ${args[ $((2 + $i)) ]}"
        echo "Mount options:        ${args[ $((3 + $i)) ]}"
        echo "Dump freqency:        ${args[ $((4 + $i)) ]}"
        echo "Fsck pass number:     ${args[ $((5 + $i)) ]}"
        echo ""
    done
fi