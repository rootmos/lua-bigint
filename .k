go() {
    #timeout 10 make -C tests
    cd tests && stack run specs -- -m '/Ascii'
    #./run -e arbbase
    ##lua go.lua
    #timeout 10 stack runhaskell go.hs
}
