#!/bin/bash
# Copyright 2022 Mitchell Kember. Subject to the MIT License.

set -eufo pipefail

cd "$(dirname "$0")/.."

# Made-up headings that are allowed in src/sicp/*.ss.
heading_exceptions=(
	"A sample simulation"
    "One-dimensional tables"
    "Primitive procedures"
)

heading_exceptions_pattern="^$(IFS=\|; echo "${heading_exceptions[*]}")$"

# shellcheck disable=SC2207
sicp_files=($(find src/sicp -type f -name "*.ss"))

# Ensure all headings in the code appear in text.md.
bad_headings=$(
    comm -13 \
        <(grep -E '^#' notes/text.md | sed -E 's/^#+ //' | sort) \
        <(grep -Eh '^\((Chapter|Section)' "${sicp_files[@]}" \
            | sed -E -e 's/^\(.+ :(.+) "(.+)".*$/\1: \2/' \
                -E -e 's/^([0-9]+\.){3}[0-9]+: //' | sort) \
        | grep -Ev "$heading_exceptions_pattern" | grep -E '^' \
) || exit 0

echo "The following headings occur in src/sicp but not in notes/text.md:"
echo "$bad_headings"
exit 1
