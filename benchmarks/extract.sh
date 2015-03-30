#!/bin/bash

main() {
    cat "$1.txt" | strip_esc | convert_to_seconds | extract "mean" | transpose > "$1-time.dat"
    cat "$1.txt" | strip_esc | extract "bytes" | transpose > "$1-alloc.dat"
    cat "$1.txt" | strip_esc | extract "retries" | transpose > "$1-retries.dat"
}

strip_esc() {
    sed -E "s/.*\[0G(mean: .*)/\1/g"
}

convert_to_seconds() {
    sed -E "s/([0-9\.]*) s,/\1/g" | 
    sed -E "s/([0-9][0-9][0-9])\.([0-9]*) ms,/0.\1\2/g" | 
    sed -E "s/([0-9][0-9])\.([0-9]*) ms,/0.0\1\2/g" | 
    sed -E "s/([0-9])\.([0-9]*) ms,/0.00\1\2/g"
}

extract() {
    grep -E "subject|$1:" | 
    sed -E 's/.*\"(.*)\"/\1/g' | 
    sed -E "s/$1: ([0-9.]+).*/\1/g" |
    sed -E 'N;s/[^0-9]*([0-9]+)\/[0-9]+\/(.*)\n[^0-9]*([0-9\.e]*)/\2 \1 \3/g'
}

transpose() {
    awk '{names[$1]++; threads[0]=0; threads[$2]++; results[$1,0]="\""$1"\""; results[$1,$2]=$3}END{for (t in threads) { printf "%s",t; for (n in names) printf " %s",results[n,t]; printf "\n" }}' | sort -n
}

main "$@"
