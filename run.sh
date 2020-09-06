#!/bin/bash

set -eufo pipefail

usage() {
    cat <<EOS
usage: $0 [--help] {all,clean,chez,chezd,guile,racket} args ...
EOS
}

clean() {
    find . -type d -name compiled -exec rm -rf {} +
}

if [[ $# -eq 0 ]]; then
    usage
    exit
fi

readonly arg=$1
shift

if ! [[ -t 1 ]]; then
    set -- "--no-color" "$@"
fi

run_chez() {
    ln -sf chez.ss src/compat/active.ss
    chez --program main.ss "$@"
}

run_chezd() {
    ln -sf chez.ss src/compat/active.ss
    chez --debug-on-exception --program main.ss "$@"
}

run_guile() {
    ln -sf guile.ss src/compat/active.ss
    # For info about --no-auto-compile, see "Known issues" in README.md.
    guile --no-auto-compile --r6rs -L . -x .ss main.ss "$@"
}

run_racket() {
    ln -sf racket.ss src/compat/active.ss
    plt-r6rs --compile ++path . main.ss
    plt-r6rs ++path . main.ss "$@"
}

case $arg in
    -h|--help) usage; exit ;;
    clean) clean; exit ;;
    chez|chezd|guile|racket) "run_$arg" "$@" ;;
    all) run_chez "$@" && run_guile "$@" && run_racket "$@" ;;
    *) usage >&2; exit 1 ;;
esac
