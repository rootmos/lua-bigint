go() {
    #timeout 10 make -C tests
    #cd tests && stack run specs -- -m '/Arbbase'
    cd tests && stack run specs -- -m '/Polynomial'
    #./run -e arbbase.test
    #lua go.lua
    #timeout 10 stack runhaskell go.hs
    #./run -t
    #make tests
}
