export QUICKCHECK_VERBOSE=1
export QUICKCHECK_MAX_SUCCESS=1000
export QUICKCHECK_SIZE=100

go() {
    #timeout 10 make -C tests
    #make tests
    cd tests && stack run specs -- -m '/Arbbase'
    #./run -e sandbox
    #./run -t
}
