#!/bin/bash

set -eufo pipefail

readonly main="src/main.ss"

usage() {
    cat <<EOS
usage: $0 [--help] {all,chez,chezd,guile,racket} args ...
EOS
}

run_all() {
    printf "Chez   ... "
    run_chez "$@"
    printf "Guile  ... "
    run_guile "$@"
    printf "Racket ... "
    run_racket "$@"
}

run_chez() {
    ln -sf chez.ss src/compat/active.ss
    chez --program "$main" "$@"
}

run_chezd() {
    ln -sf chez.ss src/compat/active.ss
    chez --debug-on-exception --program "$main" "$@"
}

run_guile() {
    ln -sf guile.ss src/compat/active.ss
    # For info about --no-auto-compile, see "Known issues" in README.md.
    guile --no-auto-compile --r6rs -L . -x .ss "$main" "$@"
}

run_racket() {
    ln -sf racket.ss src/compat/active.ss
    plt-r6rs --compile ++path . "$main"
    plt-r6rs ++path . "$main" "$@"
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

cd "$(dirname "$0")"

case $arg in
    -h|--help) usage; exit ;;
    all|chez|chezd|guile|racket) "run_$arg" "$@" ;;
    *) usage >&2; exit 1 ;;
esac
