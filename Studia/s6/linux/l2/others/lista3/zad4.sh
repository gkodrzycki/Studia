MAX=$1

if [[ -z $MAX ]]; then
    MAX=1000
fi

PRIMES=(2)
i=1
for n in $(seq 3 $MAX); do
    j=0
    while [[ $((${PRIMES[j]} * ${PRIMES[j]} )) -le $n ]]; do
        if (( $n % ${PRIMES[j]} == 0)) then
            continue 2
        fi
        j=$(($j + 1))
    done
    
    PRIMES[$((i++))]=$n
done

echo ${PRIMES[@]}
