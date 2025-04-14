#export QUICKCHECK_VERBOSE=1
#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=100

#export LUA_BITS=32
export LUA_BITS=64

TIMEOUT=10s
#TIMEOUT=0

go() {
    #timeout "$TIMEOUT" make tests
    cd tests && make build && timeout "$TIMEOUT" stack run specs -- -m '/Bignat/bignat.lua/truncating subtraction'
    #cd tests && make build && timeout "$TIMEOUT" stack run specs -- --match "/Bignat/bignat.lua/multiplication"
    #cd tests && stack runhaskell go.hs
    #./run -e sandbox
}
