#export VERBOSE=1

go() {
    #timeout 10 make -C tests
    make tests
    #cd tests && stack run specs -- -m '/Utils'
    #./run -e sandbox
    #./run -t
}
