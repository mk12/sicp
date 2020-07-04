#!/bin/bash

set -eufo pipefail

usage() {
    cat <<EOS
usage: $0 {all,chez,chezd,guile,racket} args ...
EOS
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
    guile --r6rs -L . -x .ss main.ss "$@"
}

run_racket() {
    ln -sf racket.ss src/compat/active.ss
    # TODO: --compile first?
    plt-r6rs ++path . main.ss "$@"
}

case $arg in
    -h|--help) usage; exit ;;
    chez|chezd|guile|racket) "run_$arg" "$@" ;;
    all) run_chez "$@" && run_guile "$@" && run_racket "$@" ;;
    *) usage >&2; exit 1 ;;
esac
