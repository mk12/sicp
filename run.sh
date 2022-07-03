#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

main="src/main.ss"

usage() {
    cat <<EOS
Usage: $0 [-hd] {all,chez,guile,racket} ARG ...

Run $main using the given Scheme implementation

Commands:
    all     Run with all Schemes in parallel
    chez    Run with Chez Scheme
    guile   Run with Guile
    racket  Run with Racket

Arguments:
    ARG     Forwarded to $main

Options:
    -h, --help   Show this help message
    -d, --debug  Enter debugger on uncaught exception
EOS
}

debug=
num_schemes=3

# Assuming the cursor is below $num_schemes lines of output, moves up to the
# ${1}th line and replaces its contents with $2, truncating with an ellipsis if
# necessary to fit. Then restores the cursor to its original position.
update() {
    n=$1
    line=$2
    stripped=$(sed -E 's/\x1b\[[0-9;]+m//g' <<< "$line")
    cols=$(tput cols)
    if [[ ${#stripped} -gt $cols ]]; then
        # Just ditch color since it's too hard to calculate the right size and
        # we might truncate in the middle of an escape code otherwise.
        line="${stripped::$((cols - 3))}..."
    fi
    # shellcheck disable=SC2059
    printf "\x1b[${n}A\r\x1b[2K${line}\x1b[${n}B\r"
}

# Runs ${@:3} and prints output prefixed with $2 on the ${1}th line.
monitor() {
    n=$(( num_schemes + 1 - $1 ))
    update "$n" "$2"
    prev=
    # Use a pipe instead of process substitution to propagate the exit status.
    "${@:3}" 2>&1 | while read -r line; do
        # Guile logs compilation info to stderr prefixed with ";;;". Sometimes
        # the order gets messed up with 2>&1 so that one of these logs comes
        # last instead of the main.ss output. To avoid that, we skip printing
        # a log line if it comes after a non-log line.
        [[ "$prev" != ';;;'* && "$line" = ';;;'* ]] && continue
        update "$n" "$2$line"
        prev=$line
    done
}

run_all() {
    head -c $num_schemes /dev/zero | tr '\0' $'\n'
    pids=()
    monitor 1 "Chez   ... " run_chez "$@" & pids+=($!)
    monitor 2 "Guile  ... " run_guile "$@" & pids+=($!)
    monitor 3 "Racket ... " run_racket "$@" & pids+=($!)
    status=0
    for pid in "${pids[@]}"; do
        wait "$pid" || status=$?
    done
    return $status
}

run_chez() {
    flag=
    [[ $debug = true ]] && flag=--debug-on-exception
    chez --libdirs .:src/compat/chez --compile-imported-libraries $flag \
        --program "$main" "$@"
}

run_guile() {
    compat=src/compat/guile
    cmd=(guile -q --r6rs -L . -L "$compat" -x .ss -l "$compat/src/init.ss")
    if [[ $debug = true ]]; then
        "${cmd[@]}" -l "$compat/src/debug.ss" -- "$@"
    else
        "${cmd[@]}" -s "$main" "$@"
    fi
}

run_racket() {
    if [[ $debug = true ]]; then
        echo "$0: debugging not supported for racket" >&2
        exit 1
    fi
    racket -q --search . --search src/compat/racket --make "$main" "$@"
}

ours=true
scheme=
fwd=()

# This logic is a bit complicated so that we can recognize -d/--debug anywhere,
# while passing everything else on to main.ss, unless there is a -- argument
# that explicitly divides run.sh options from main.ss options.
for arg; do
    if [[ $ours = true ]]; then
        case $arg in
            -d|--debug) debug=true; continue ;;
            --) ours=false; continue ;;
            *)
                if [[ -z "$scheme" ]]; then
                    case $arg in
                        -h|--help|help) usage; exit ;;
                        *) scheme=$arg; continue ;;
                    esac
                fi
                ;;
        esac
    fi
    fwd+=("$arg")
done

# Detect whether to enable/disable color here since it's easier than in Scheme.
if ! [[ -t 1 ]] || [[ -n ${NO_COLOR+x} ]]; then
    fwd+=(--no-color)
fi

case $scheme in
    all|chez|guile|racket) "run_$scheme" "${fwd[@]+"${fwd[@]}"}" ;;
    *) usage >&2; exit 1 ;;
esac
