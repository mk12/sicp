#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

usage() {
    cat <<EOS
Usage: $0

Watch files and live-reload docs

Initially it will let you choose an HTML in docs/ using fzf, and then watch for
changes to all inputs that affect that file. Press any key to choose a different
HTML file. When an input file changes, it rebuilds the page and refreshes it in
the browser. This only works on macOS with Safari.
EOS
}

entr_pid1=
entr_pid2=

inputs=
output=docs/index.html

refresh() {
    make "$output" && open -g "$output"
}

restart_render() {
    rm -f render.sock
    # Normally render.sock is used as an INTERMEDIATE target for building docs
    # when a server isn't already running. However, if we ask for it explicitly,
    # it will not get deleted at the end as usual.
    make render.sock
}

# Make functions work in entr.
export SHELL=/bin/bash
export output
export -f refresh restart_render

choose_output() {
    new_output=$(find docs -type f -name "*.html" | fzf)
    output=${new_output:-$output}
    inputs=()
    case ${output#docs/} in
        index.html)
            inputs+=("notes/index.md") ;;
        text/*)
            inputs+=("notes/text.md") ;;
        lecture/*)
            inputs+=("notes/lecture.md") ;;
        exercise/index.html|exercise/language.html)
            inputs+=("notes/exercise.md") ;;
        exercise/*)
            n=${output#docs/exercise/}
            n=${n%%/*}
            inputs+=("src/sicp/chapter-$n.ss")
            ;;
    esac
    inputs+=(
        "tools/docgen.c"
        "tools/highlight.c"
        "docs/style.css"
    )
    while read -r file; do
        inputs+=("$file")
    done < <(find notes/pandoc -type f)
}

kill_entr() {
    [[ -n "$entr_pid1" ]] && kill "$entr_pid1"
    [[ -n "$entr_pid2" ]] && kill "$entr_pid2"
}

watch() {
    kill_entr
    printf '%s\n' "${inputs[@]}" | entr -ns 'refresh' & entr_pid1=$!
    echo tools/render.ts | entr -nsp 'restart_render && refresh' & entr_pid2=$!
}

cleanup() {
    kill_entr
    rm -f render.sock
}

if [[ $# -gt 0 ]]; then
    usage
    exit 0
fi

if [[ "$(uname -s)" != Darwin ]]; then
    echo "error: this script only works on macOS" >&2
    exit 1
fi

cd "$(dirname "$0")"
trap cleanup EXIT
restart_render
while :; do
    choose_output
    watch
    read -r -n1
done
