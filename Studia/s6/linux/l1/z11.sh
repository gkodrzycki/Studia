display() {
    input_string="$1"
    colored_string=$(echo "$input_string" | 
    sed -E 's/(R)/\o033[91mR\o033[0m/g' | 
    sed -E 's/(G)/\o033[38;5;118mG\o033[0m/g' | 
    sed -E 's/(B)/\o033[96mB\o033[0m/g')
    echo "$colored_string"
}

find_first_pattern() {
    word=$1
    best_pos=${#word}
    first_pattern=""
    
    for pattern in "GR" "BR" "RB" "BG" "GB" "RG"; do
        if echo "$curr_state" | grep -q "$pattern"; then
            pos=$(echo "$curr_state" | grep -b -o "$pattern" | head -n 1 | cut -d: -f1)
            pos=$((pos + 0))
            if [ "$best_pos" -gt "$pos"  ]; then
                best_pos="$pos"
                first_pattern=$pattern
            fi
        fi
    done

    if [ "$first_pattern" == "" ]; then
        for pattern in "GG" "BB" "RR"; do
            if echo "$curr_state" | grep -q "$pattern"; then
                first_pattern=$pattern
            fi
        done
    fi
    echo "$first_pattern"
}

find_opposite() {
    pair=$1
    opposite=""
    case "$pair" in
        "RG") opposite="BB" ;;
        "GR") opposite="BB" ;;
        "RB") opposite="GG" ;;
        "BR") opposite="GG" ;;
        "GB") opposite="RR" ;;
        "BG") opposite="RR" ;;
        "BB") opposite="BB" ;;
        "GG") opposite="GG" ;;
        "RR") opposite="RR" ;;
        *) echo "Unknown pair" ;;
    esac

    echo "$opposite"
}

curr_state="$1"
display $curr_state

while true; do
    first_pattern=$(find_first_pattern "$curr_state")
    # echo "FIRST " $first_pattern
    opposite=$(find_opposite "$first_pattern")
    # echo "OPPOSITE " $opposite
    new_state=$(echo "$curr_state" | sed "s/${first_pattern}/${opposite}/")
    if [ $curr_state != $new_state ]; then
        display "$new_state"
        curr_state="$new_state"
    else
        break
    fi
done   for pattern in "GR" "BR" "RB" "BG" "GB" "RG"; do
        if echo "$curr_state" | grep -q "$pattern"; then
            pos=$(echo "$curr_state" | grep -b -o "$pattern" | head -n 1 | cut -d: -f1)
            pos=$((pos + 0))
            if [ "$best_pos" -gt "$pos"  ]; then
                best_pos="$pos"
                first_pattern=$pattern
