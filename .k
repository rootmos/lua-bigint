#export QUICKCHECK_VERBOSE=1

#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=100

export QUICKCHECK_MAX_SUCCESS=10
export QUICKCHECK_SIZE=5

#export LUA_BITS=32
export LUA_BITS=64

gogo() {
    #make tests
    cd tests && stack run specs
    #cd tests && make build && stack run specs -- -m '/Bignat/bignat.lua/division'
    #cd tests && make build && stack run specs -- -m '/Bignat/bignat.lua/division/should divide same'
    #cd tests && make build && stack run specs -- -m '/Polynomial/polynomial.lua'
    #cd tests && make build && stack run specs -- -m '/Internal'
    #cd tests && stack runhaskell go.hs
    #./run src/sandbox.lua
    #./run examples/bignat.lua
}

#TIMEOUT=10s
TIMEOUT=0

go() {
    timeout "$TIMEOUT" k gogo
}
