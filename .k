#export QUICKCHECK_VERBOSE=1
#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=100

#export LUA_BITS=32
export LUA_BITS=64

go() {
    #timeout 10 make -C tests
    #make tests
    cd tests && make build && timeout 5 stack run specs -- -m '/Bignat'
    #./run -e sandbox
    #./run -t
}
