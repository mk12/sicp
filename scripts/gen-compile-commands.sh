#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

cd "$(dirname "$0")/.."

echo "["

for tool; do
    command=$(make -Bn "$tool" | grep 'tools/' | sed 's/^cc /clang /')
    file=${command##* }
    comma=,
    [[ "$tool" = "${*:$#:1}" ]] && comma=
    cat <<EOS
    {
        "directory": "$(pwd)",
        "file": "$file",
        "command": "$command"
    }$comma
EOS
done

echo "]"
