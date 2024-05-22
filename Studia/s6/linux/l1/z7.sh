zad1(){
	find / \
    -type f \
    -regextype posix-extended \
    -regex '.*\.so(\.[0-9]+)*$' \
    -print \
    2>/dev/null
}

zad2(){
    find / \
    -type l \
    -exec file {} + \
    2>/dev/null \
    | grep -E 'symbolic link to .*\.so(\.[0-9]+)*$' \
    | awk -F: '{print $1}'
}

zad3(){
    find / \
    -type f \
    -exec du -b {} + \
    2>/dev/null \
    | grep -E '\.so(\.[0-9]+)*$' \
    | awk 'BEGIN { count = 0; sum = 0 } { sum += $1; count++ } \
    END { sum_gb = sum / (1024 * 1024 * 1024); \
    avg_size = (sum / count) / (1024 * 1024); \
    print "Liczba pasujących plików:", count; \
    print "Sumaryczny rozmiar:", sum_gb, "GB"; \
    print "Średni rozmiar:", avg_size, "MB" }'
}

zad4() {
    find / \
    -type f \
    -regextype posix-extended \
    -regex '.*\.so(\.[0-9]+)*$' \
    -exec dirname {} + \
    2>/dev/null \
    | sort -u
}

zad5() {
    find / \
    -type l \
    -exec file {} + \
    2>/dev/null \
    | grep -E 'symbolic link to .*\.so(\.[0-9]+)*$' \
    | awk -F: '{print $1}' \
    | awk -F/ '{$NF=""; OFS="/"; print}' \
    | sort -u
}

"$1"