#!/bin/bash

PRIMES=(2)
i=1
max=$1
for n in $(seq 3 $max); do
    j=0
    while [[ $(( ${PRIMES[j]} ** ${PRIMES[j]} )) -le $n ]]; do
        if [[ $(( $n % ${PRIMES[j]} )) -eq 0 ]]; then
            continue 2
        fi
        j=$(( $j + 1 ))
    done
    PRIMES[$((i++))]="$n"
done
echo "${PRIMES[@]}"
