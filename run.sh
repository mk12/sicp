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

case $1 of
    -h|--help) usage; exit ;;
    chez) chez --program main.ss ;;
    guile) guile --r6rs -L . -x .ss main.ss ;;
    racket) plt-r6rs ++path . main.ss ;;
    *) usage >&2; exit 1 ;;
esac
