#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

site=https://mitpress.mit.edu
prefix=$site/sites/default/files/sicp/full-text/book/book-Z-H
num_sec=(0 3 5 5 4 5)
offset=(0 0 3 8 13 17)

echo "Downloading files from $site into $1"
mkdir -p "$1"

for c in 1 2 3 4 5; do
    echo "Chapter $c ..."
    for s in $(seq 0 "${num_sec[$c]}"); do
        o=${offset[$c]}
        n=$((8 + c + o + s))
        curl -s "$prefix-$n.html" > "$1/$c.$s.html" &
    done
    wait
done
