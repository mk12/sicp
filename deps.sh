#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

usage() {
    cat <<EOS
Usage: $0 {check,install}

Manage dependencies for the SICP Study project

Commands:
    check    Check if dependencies are installed
    install  Install missing dependencies (macOS only)
EOS
}

lua_version=$(make print-LUA_VERSION)
platform=
warned=false

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
    command -v "$1" &> /dev/null
}

racket_file_installed() {
    [[ -z "$(racket -I "$1" -e '' 2>&1)" ]]
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
    # Note: Lua is only needed for its headers, to compile the C libraries in
    # tools/lua. But we assume you just install the entire Lua package.
    say "checking programs"
    for cmd in chez guile racket pandoc lua$lua_version deno svgbob python3 \
        vnu shellcheck clang-format
    do
        installed "$cmd" || warn "$cmd not installed"
    done
    say "checking racket packages"
    if installed racket; then
        for x in compiler-lib:compiler/sig.rkt r6rs-lib:r6rs; do
            racket_file_installed "${x#*:}" || warn "${x%:*} not installed"
        done
    else
        warn "cannot check racket packages: need racket"
    fi
    say "checking pandoc lua version"
    if installed pandoc; then
        v=$(pandoc --lua-filter <(echo 'print(_VERSION)') <<< '' \
            | grep -o '\d\.\d')
        if [[ "$v" != "$lua_version" ]]; then
            warn "pandoc is built with lua $v; expected lua $lua_version"
        fi
    else
        warn "cannot check pandoc lua version: need pandoc"
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
    for x in compiler-lib:compiler/sig.rkt r6rs-lib:r6rs; do
        if ! racket_file_installed "${x#*:}"; then
            raco pkg install --auto --no-docs "${x%:*}"
        fi
    done
}

install_macos_docs() {
    lua="lua@$lua_version"
    lua_dir=$(brew --prefix "$lua")
    install_macos_formulas pandoc deno "$lua:$lua_dir/bin/lua"
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
    install_macos_formulas python3 vnu clang-format shellcheck
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

case $# in
    1)
        case $1 in
            -h|--help|help) usage ;;
            check|install) "$1" ;;
            *) die "$1: invalid command" ;;
        esac
        ;;
    *) usage >&2; exit 1 ;;
esac

if [[ $warned = true ]]; then
    exit 1
fi
