#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

cd "$(dirname "$0")/.."

echo "["

for output; do
    command=$(make -Bn "$output" | tail -n1 | sed 's/^cc /clang /')
    file=${command##* }
    comma=,
    [[ "$output" = "${*:$#:1}" ]] && comma=
    cat <<EOS
    {
        "directory": "$(pwd)",
        "file": "$file",
        "output": "$output",
        "command": "$command"
    }$comma
EOS
done

echo "]"
