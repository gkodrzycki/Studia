
trap 'clear && exit' INT

while true; do
    avail=$(cat /proc/sys/kernel/random/entropy_avail)
    max=$(cat /proc/sys/kernel/random/poolsize)
    echo -n -e "Available entropy: $avail /$max\r"
    
    read -t 1 -n 1 && break 1

done
clear