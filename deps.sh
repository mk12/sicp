#!/bin/bash
# shellcheck disable=SC2059

set -eufo pipefail

readonly lua_version=5.3
platform=
warned=false

red="\x1b[31;1m"
green="\x1b[32;1;1m"
yellow="\x1b[33;1m"
blue="\x1b[34m"
magenta="\x1b[35;1m"
reset="\x1b[0m"

if ! [[ -t 1 && -t 2 ]] || [[ -n ${NO_COLOR+x} ]]; then
    red=
    green=
    yellow=
    blue=
    magenta=
    reset=
fi

usage() {
    cat <<EOS
usage: $0 [--help] {check,install}

This script manages dependencies for the SICP Study project. The 'check' command
checks if all dependencies are present. The 'install' commands installs missing
dependencies automatically (but prompts before doing anything).
EOS
}

say() {
    printf "$blue> $*$reset\n" "$*"
}

run() {
    printf "$green> %s$reset\n" "$*"
    "$@"
}

ask() {
    printf "$magenta> $1 [y/N]$reset " "$1"
    read -r
    case $REPLY in
        y|Y) ;;
        *) return 1 ;;
    esac
}

warn() {
    printf "${yellow}warning:$reset %s\n" "$*"
    warned=true
}

die() {
    printf "${red}error:$reset %s\n" "$*" >&2
    exit 1
}

get_platform() {
    platform=$(uname -s)
    case $platform in
        Darwin) platform=macos ;;
        *) return 1 ;;
    esac
}

check() {
    say "checking programs"
    for cmd in chez guile racket pandoc deno; do
        command -v $cmd &> /dev/null || warn "$cmd not installed"
    done
    say "checking pandoc lua version"
    if command -v pandoc &> /dev/null; then
        v=$(pandoc --lua-filter <(echo 'print(_VERSION)') <<< '' \
            | grep -o '\d\.\d')
        if [[ "$v" != "$lua_version" ]]; then
            warn "pandoc is built with lua $v; expected lua $lua_version"
        fi
    fi
    if get_platform; then
        check_$platform
    else
        warn "skipping some check not supported on platform $platform"
    fi
    if [[ $warned == true ]]; then
        return 1
    fi
}

check_macos() {
    say "checking lua libraries"
    if ! "$(brew --prefix lua@5.3)/bin/lua" -l posix <<< "" &> /dev/null; then
        warn "luaposix not installed"
    fi
}

install() {
    get_platform || die "unsupported platform $platform"
    install_${platform}_prep
    if ask "install all supported schemes?"; then
        install_${platform}_scheme
    fi
    if ask "install dependencies for buiding docs?"; then
        install_${platform}_docs
    fi
}

install_macos_prep() {
    if ! command -v brew &> /dev/null; then
        die "try again after installing https://brew.sh"
    fi
}

install_macos_cmds() {
    args=()
    for cmd in "$@"; do
        if command -v "${cmd#*:}" &> /dev/null; then
            say "note: ${cmd#*:} is already installed"
        else
            args+=("${cmd%:*}")
        fi
    done
    if [[ ${#args[@]} -gt 0 ]]; then
        run brew install "${args[@]}"
    fi
}

install_macos_scheme() {
    install_macos_cmds chezscheme:chez guile racket
}

install_macos_docs() {
    lua="lua@$lua_version"
    lua_dir=$(brew --prefix $lua)
    install_macos_cmds pandoc "$lua:$lua_dir/bin/lua" luarocks deno
    if "$lua_dir/bin/lua" -l posix <<< "" &> /dev/null; then
        say "luaposix is already installed"
    else
        run luarocks --lua-dir="$lua_dir" install luaposix
    fi
}

case ${1:-} in
    ""|-h|--help) usage ;;
    check|install) "$1" ;;
    *) die "$1: invalid command" ;;
esac
