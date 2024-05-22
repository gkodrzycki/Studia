IFS=$'\n'


folder=$1
max_size=$2

if [[ -z $folder ]]; then 
    folder="."
fi

if [[ -z $max_size ]]; then
    max_size=250000
fi

# echo $folder
# echo $max_size

result=$(find $folder ! -path . -printf "%T+ %i %s\n" | sort -r)


sum_bytes=0
for res in $result; do
    args=($(echo $res | tr " " "\n" ))
    sum_bytes=$(($sum_bytes+${args[2]} ))
done

echo $sum_bytes

for res in $result; do
    if [[ $sum_bytes -lt $max_size ]]; then
        break
    fi

    args=($(echo $res | tr " " "\n" ))
    sum_bytes=$(($sum_bytes-${args[2]}))
    echo $(find $folder -inum ${args[1]} -printf "%f")
    
done

