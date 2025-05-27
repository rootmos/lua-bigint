#!/bin/bash

set -o nounset -o pipefail -o errexit

SCRIPT_DIR=$(readlink -f "$0" | xargs dirname)
ROOT=$(readlink -f "$SCRIPT_DIR/..")

OUTPUT_DIR=$(readlink -f "${1-"$SCRIPT_DIR/api"}")
echo 1>&2 "output: $OUTPUT_DIR"

if [ -z "${WORKDIR-}" ]; then
    WORKDIR=$(mktemp -d)
    trap 'rm -rf $WORKDIR' EXIT
else
    mkdir -p "$WORKDIR"
fi
WORKDIR=$(readlink -f "$WORKDIR")
cd "$WORKDIR"

DIST=${DIST-"$WORKDIR/dist"}
if [ ! -d "$DIST" ]; then
    "$ROOT/mk-dist" -L "$DIST"
fi

FILEs=()
FILEs+=("$DIST/bigint.lua")
#FILEs+=("$DIST/bignat.lua")

echo 1>&2 "files: ${FILEs[*]}"

luadox --out="$OUTPUT_DIR" "${FILEs[@]}"
