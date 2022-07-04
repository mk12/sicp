#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

usage() {
    cat <<EOS
Usage: $0

Watch files and live-reload docs

Initially it will let you choose an HTML in docs/ using fzf, and then watch for
changes to all inputs that affect that file. Press enter to choose a different
HTML file. When an input file changes, it rebuilds the page and refreshes it in
the browser. This only works on macOS with Safari.
EOS
}

entr_pid1=
entr_pid2=

inputs=
output=docs/index.html

die() {
    echo "$0: $*" >&2
    exit 1
}

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
    if [[ -n "$entr_pid1" ]]; then
        kill "$entr_pid1" || :
    fi
    if [[ -n "$entr_pid2" ]]; then
        kill "$entr_pid2" || :
    fi
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

main() {
    if [[ "$(uname -s)" != Darwin ]]; then
        die "this script only works on macOS"
    fi

    cd "$(dirname "$0")/.."
    trap cleanup EXIT
    restart_render
    while :; do
        choose_output
        watch
        read -r
    done
}

case $# in
    0) main ;;
    1)
        case $1 in
            -h|--help) usage ;;
            *) usage >&2; exit 1 ;;
        esac
        ;;
    *) usage >&2; exit 1 ;;
esac
