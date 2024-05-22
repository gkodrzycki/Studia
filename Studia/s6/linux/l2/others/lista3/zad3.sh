#!/bin/bash

if [[ "$#" -eq 1 ]]; then
    max=$1
else
    max=1000
fi 

filter() {
    local number
    read -r number
    if [[ -z $number ]]; then
        exit
    fi

    echo "$number"
    while read -r num; do
        if [[ ! $(($num % $number)) -eq 0 ]]; then
            echo $num
        fi
    done | filter &
    
}

primes(){
    for n in $(seq 2 $max); do
        echo $n
    done | filter &
}

primes 
