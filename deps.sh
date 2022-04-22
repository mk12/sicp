#!/bin/bash

set -eufo pipefail

readonly lua_version=5.4
platform=
warned=false

usage() {
    cat <<EOS
usage: $0 [--help] {check,install}

This script manages dependencies for the SICP Study project. The 'check' command
checks if all dependencies are present. The 'install' commands installs missing
dependencies automatically (but prompts before doing anything).
EOS
}

say() {
    echo "*** $*"
}

run() {
    echo "> $*"
    "$@"
}

ask() {
    echo -n "*** $* [y/N] "
    read -r
    case $REPLY in
        y|Y) ;;
        *) return 1 ;;
    esac
}

warn() {
    echo "warning: $*" >&2
    warned=true
}

die() {
    echo "error: $*" >&2
    exit 1
}

installed() {
    command -v "$1" &> /dev/null || return 1
}

set_luarocks_env() {
    eval "$(luarocks --lua-version=$lua_version path)"
}

pandoc_lua_library_installed() {
    if ! pandoc --lua-filter <(echo "require(\"$1\")") <<< '' &> /dev/null; then
        return 1
    fi
}

say_already_installed() {
    say "note: $1 is already installed"
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
    for cmd in chez guile racket pandoc deno svgbob vnu shellcheck clang-format
    do
        installed $cmd || warn "$cmd not installed"
    done
    say "checking racket r6rs"
    if ! racket -I r6rs -e '' &> /dev/null; then
        warn "racket r6rs not installed"
    fi
    say "checking pandoc lua version"
    if installed pandoc; then
        v=$(pandoc --lua-filter <(echo 'print(_VERSION)') <<< '' \
            | grep -o '\d\.\d')
        if [[ "$v" != "$lua_version" ]]; then
            warn "pandoc is built with lua $v; expected lua $lua_version"
        fi
    fi
    say "checking lua libraries"
    if installed pandoc && installed luarocks; then
        set_luarocks_env
        if ! pandoc_lua_library_installed posix; then
            warn "luaposix not installed"
        fi
    else
        warn "cannot check lua libraries: need pandoc and luarocks"
    fi
}

install() {
    get_platform || die "unsupported platform: $platform"
    install_${platform}_prep
    if ask "install all supported schemes?"; then
        install_${platform}_scheme
    fi
    if ask "install dependencies for buiding docs?"; then
        install_${platform}_docs
    fi
    if ask "install other development tools?"; then
        install_${platform}_other
    fi
}

install_macos_prep() {
    if ! installed brew; then
        die "try again after installing https://brew.sh"
    fi
}

install_macos_scheme() {
    install_macos_formulas chezscheme:chez guile minimal-racket:racket
    if ! racket -I r6rs -e '' &> /dev/null; then
        raco pkg install --auto --no-docs r6rs-lib
    fi
}

install_macos_docs() {
    lua="lua@$lua_version"
    lua_dir=$(brew --prefix $lua)
    install_macos_formulas pandoc "$lua:$lua_dir/bin/lua" luarocks deno
    set_luarocks_env
    if pandoc_lua_library_installed posix; then
        say_already_installed luaposix
    else
        run luarocks --lua-dir="$lua_dir" --local install luaposix
    fi
    if installed svgbob; then
        say_already_installed svgbob
    else
        if ! installed cargo; then
            die "try again after installing https://rustup.rs (need for svgbob)"
        fi
        # Latest commit as of 2021-03-27.
        svgbob_git=https://github.com/ivanceras/svgbob
        svgbob_rev=3a2fdd784044130909ad992bede02b971e4b8721
        run cargo install svgbob_cli --git $svgbob_git --rev $svgbob_rev
        if ! installed svgbob; then
            warn "svgbob executable not found; is ~/.cargo/bin in your PATH?"
        fi
    fi
}

install_macos_other() {
    install_macos_formulas vnu clang-format shellcheck
}

install_macos_formulas() {
    args=()
    for cmd in "$@"; do
        if installed "${cmd#*:}"; then
            say_already_installed "${cmd#*:}"
        else
            args+=("${cmd%:*}")
        fi
    done
    if [[ ${#args[@]} -gt 0 ]]; then
        run brew install "${args[@]}"
    fi
}

case ${1:-} in
    ""|-h|--help) usage ;;
    check|install) "$1" ;;
    *) die "$1: invalid command" ;;
esac

if [[ $warned == true ]]; then
    exit 1
fi
