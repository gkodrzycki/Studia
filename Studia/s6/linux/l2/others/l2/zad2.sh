#!/bin/bash


path=$1
while [[ -z $path ]]; do
    echo -n "Enter path: "
    read path
done
path=$(realpath $path)

result=$(grep -E "^UUID=[A-Fa-f0-9\-]+[ ]*$path[ ]+.*" /etc/fstab)
if [[ ! -z $result ]]; then
    echo $result | awk '{print "Device:           " $1 \
                             "\nFilesystem type:  " $3 \
                             "\nMount options:    " $4 \
                             "\nDump Frequency:   " $5 \
                             "\nFsck pass number: " $6}'
fi
