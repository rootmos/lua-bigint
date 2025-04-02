go() {
    #timeout 10 make -C tests
    make tests
    #cd tests && stack run specs -- -m '/Arbbase'
    #cd tests && stack run specs -- -m '/Polynomial'
    #./run -e arbbase.test
    #./run -t
}
