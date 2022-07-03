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
    mkfifo render.fifo
    deno run --unstable --allow-{read,write,run} notes/pandoc/render.ts \
        render.{sock,fifo} &
    : < render.fifo
    rm render.fifo
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
    done < <(find notes/pandoc -type f -not -name render.ts)
}

watch() {
    [[ -n "$entr_pid1" ]] && kill "$entr_pid1"
    [[ -n "$entr_pid2" ]] && kill "$entr_pid2"
    printf '%s\n' "${inputs[@]}" | entr -ns 'refresh' &
    entr_pid1=$!
    echo notes/pandoc/render.ts | entr -nsp 'restart_render && refresh' &
    entr_pid2=$!
}

cleanup() {
    for pid in "$entr_pid1" "$entr_pid2"; do
        if [[ -n "$pid" ]] && kill -s 0 "$pid"; then
            kill "$pid" || :
        fi
    done
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
