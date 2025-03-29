go() {
    #make -C tests
    cd tests && stack run specs -- -m '/ascii.lua'
    #./run -e arbbase
    #lua go.lua
    #stack runhaskell go.hs
}
