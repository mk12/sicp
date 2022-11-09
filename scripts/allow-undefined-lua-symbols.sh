#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

# When we compile our Lua C shared libraries, the linker complains that all the
# Lua symbols are undefined. This is expected: they should be provided by the
# process that loads the shared library (in our case, Pandoc, which embeds Lua).
# Therefore, we grab all those symbols from the object file and explicitly tell
# clang to tell ld (-Wl) to allow the symbols to be undefined (-U).

nm -gpu "$1" | grep '^_lua' | sed 's/^/-Wl,-U,/'
