#!/bin/bash

set -eufo pipefail

render_pid=
entr_pid=

ensure_render() {
    [[ -S render.sock ]] && return
    mkfifo render.fifo
    deno run --unstable --allow-{read,write,run} notes/pandoc/render.ts \
        render.{sock,fifo} &
    render_pid=$!
    : < render.fifo
    rm render.fifo
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

if [[ "$(uname -s)" != Darwin ]]; then
    echo "error: this script only works on macOS" >&2
    exit 1
fi

cd "$(dirname "$0")"
trap 'cleanup' EXIT
ensure_render

while :; do
    restart
    read -r -n1
done
