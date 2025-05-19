#export QUICKCHECK_VERBOSE=1
#export QUICKCHECK_VERBOSE_SHRINKING=1
#export QUICKCHECK_NO_SHRINKING=1

#export QUICKCHECK_MAX_SUCCESS=1000
#export QUICKCHECK_SIZE=$((RANDOM%500))

export QUICKCHECK_MAX_SUCCESS=10
export QUICKCHECK_SIZE=5

#export LUA_BITS=32
export LUA_BITS=64

gogo() {
    #make tests
    #cd tests && make build
    cd tests && stack run specs -- -m "/Bignat/"
    #cd tests && stack run specs -- -m '/Bigint'
    #cd tests && stack run specs -- --match "/Bigint/integer division truncated towards negative infinity/I.div(a,b)/" --seed 819336231
    #cd tests && stack run specs -- --match "/Bigint/greater than or equal/a:ge(b)/" --seed 799366864
    #cd tests && stack run specs -- --match "/Bigint/divmod function/{a:divmod(b)}/" --seed 581370833
    #cd tests && stack run specs -- --match "/Bigint/divmod function/{a:divmod(b)}/" --seed 211560388
    #cd tests && stack run specs -- --match "/Bigint/quotrem function"
    #cd tests && stack run specs -- --match "/Bigint/remainder after integer division truncated towards negative infinity/a:mod(b)/" --seed 1385156359
    #cd tests && stack run specs -- -m '/LuaUtils'
    #./run examples/bignat.lua
    #./mk-dist && tail -n10 dist/bignat.lua
    #stack runghc -- ../go.hs
}

#TIMEOUT=10s
TIMEOUT=0

go() {
    timeout "$TIMEOUT" k gogo
}
