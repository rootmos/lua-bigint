#export QUICKCHECK_VERBOSE=1

#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=100

export QUICKCHECK_MAX_SUCCESS=10
export QUICKCHECK_SIZE=5

#export LUA_BITS=32
export LUA_BITS=64

gogo() {
    #make tests
    #cd tests && stack run specs
    #cd tests && make build
    cd tests && make build && stack run specs -- -m '/Bignat2'
    #cd tests && make build && stack run specs -- -m '/Utils'
    #./run examples/bignat.lua
    #./mk-dist && tail -n10 dist/bignat.lua
}

#TIMEOUT=10s
TIMEOUT=0

go() {
    timeout "$TIMEOUT" k gogo
}
