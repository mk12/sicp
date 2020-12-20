#!/bin/bash

set -eufo pipefail

run() {
    echo "+ $*"
    "$@"
}

cd "$(dirname "$0")"

if [[ $# -eq 0 ]]; then
    make linter
    find . -type f \( -name "*.md" -o -name "*.sh" -o -name "*.ss" \) \
        | entr -p "./$(basename "$0")" /_
else
    case $1 in
        */notes/*.md)
            base=$(basename "$1")
            run make "docs/${base%.*}.html"
            run ./linter "$1"
            ;;
        *.sh) run shellcheck "$1" ;;
        *.ss|*.md) run ./linter "$1" ;;
    esac
fi
