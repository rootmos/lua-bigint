#!/bin/bash

set -o nounset -o pipefail -o errexit
SCRIPT_DIR=$(readlink -f "$0" | xargs dirname)
LUA_BITS=${LUA_BITS-64}
LUA="$SCRIPT_DIR/deps/lua/root/lua$LUA_BITS/bin/lua"

if [ ! -f "$LUA" ]; then
    "$SCRIPT_DIR/deps/lua/build"
fi

exec "$LUA" "$@"
