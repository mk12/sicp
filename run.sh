#!/bin/bash

set -eufo pipefail

usage() {
    cat <<EOS
usage: $0 {chez,guile,racket} args ...
EOS
}

if [[ $# -eq 0 ]]; then
    usage
    exit
fi

readonly arg=$1
shift

case $arg in
    -h|--help) usage; exit ;;
    chez)
        ln -sf chez.ss src/compat/impl.ss
        chez --program main.ss "$@"
        ;;
    guile)
        ln -sf guile.ss src/compat/impl.ss
        guile --r6rs -L . -x .ss main.ss "$@"
        ;;
    racket)
        ln -sf racket.ss src/compat/impl.ss
        plt-r6rs ++path . main.ss "$@"
        ;;
    *) usage >&2; exit 1 ;;
esac
