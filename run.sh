#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

main="src/main.ss"

usage() {
    cat <<EOS
Usage: $0 [-h] {all,chez,guile,racket} ARG ...

Run $main using the given Scheme implementation.
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

chez_cmd=(
    chez --libdirs .:src/compat/chez --compile-imported-libraries
    --program "$main"
)

run_chez() {
    "${chez_cmd[@]}" "$@"
}

run_chezd() {
    "${chez_cmd[@]}" --debug-on-exception "$@"
}

run_guile() {
    guile -q --r6rs -L . -L src/compat/guile -x .ss \
        -l src/compat/guile/src/init.ss "$main" "$@"
}

run_racket() {
    racket --search . --search src/compat/racket --make "$main" "$@"
}

if [[ $# -eq 0 ]]; then
    usage
    exit
fi

arg=$1
shift

if ! [[ -t 1 ]] || [[ -n ${NO_COLOR+x} ]]; then
    set -- "--no-color" "$@"
fi

cd "$(dirname "$0")"

case $arg in
    -h|--help|help) usage; exit ;;
    all|chez|chezd|guile|racket) "run_$arg" "$@" ;;
    *) usage >&2; exit 1 ;;
esac
