#!/bin/bash

declare -a songs

function print_menu {
for path in "${songs[@]}"; do
    mp3info -p "%l (%a): %t\n" "$path" 
done
}

function find_songs {
    OIFS=$IFS
    IFS=$'\n'
    idx=$((1))
    for mp3path in $(find "$1" -regextype egrep -regex ".*\.mp3" 2>/dev/null); do
        songs[$idx]="$mp3path"
        idx=$(($idx+1))
    done
    IFS=$OIFS
}


find_songs "$1"
PS3="Choose a number to play> "
IFS=$'\n'
select fname in $(print_menu);
do
    if [[ -z "$fname" ]] then
        echo "Wrong answer"
    else
        echo "Playing $fname"
	    mpg123 --quiet "${songs[$REPLY]}"
    fi
done

