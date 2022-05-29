#!/bin/bash

set -eufo pipefail

render_pid=
entr_pid=

run_render() {
    deno run --no-check --unstable --allow-read --allow-write --allow-run \
        notes/pandoc/render.ts render.sock "$@"
}

ensure_render() {
    [[ -S render.sock ]] && return
    run_render &
    render_pid=$!
    run_render --wait
}

restart() {
    [[ -n "$entr_pid" ]] && kill "$entr_pid"
    output=$(find docs -type f -name "*.html" | fzf)
    [[ -z "$output" ]] && exit
    case ${output#docs/} in
        index.html) input=notes/index.md ;;
        text/*) input=notes/text.md ;;
        lecture/*) input=notes/lecture.md ;;
        exercise/index.html|exercise/language.html) input=notes/exercise.md ;;
        exercise/*)
            n=${output#docs/exercise/}
            n=${n%%/*}
            input=src/sicp/chapter-$n.ss
            ;;
    esac
    entr -ns "make '$output' && open -g '$output'" <<< "$input" &
    entr_pid=$!
}

cleanup() {
    for pid in "$render_pid" "$entr_pid"; do
        [[ -z "$pid" ]] && continue
        kill "$pid" || :
    done
}

cd "$(dirname "$0")"
trap 'cleanup' EXIT
ensure_render

while :; do
    restart
    read -r -n1
done
