#export QUICKCHECK_VERBOSE=1
#export QUICKCHECK_VERBOSE_SHRINKING=1
#export QUICKCHECK_NO_SHRINKING=1

#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=$((RANDOM%500))

export QUICKCHECK_MAX_SUCCESS=10
export QUICKCHECK_SIZE=5

#export LUA_BITS=32
export LUA_BITS=64

gogo() {
    #make tests
    #cd tests && make build
    #cd tests && stack run specs -- -m '/Bignat'
    cd tests && stack run specs -- -m '/Bigint'
    #cd tests && stack run specs -- -m '/LuaUtils'
    #./run examples/bignat.lua
    #./mk-dist && tail -n10 dist/bignat.lua
}

TIMEOUT=10s
#TIMEOUT=0

go() {
    timeout "$TIMEOUT" k gogo
}
