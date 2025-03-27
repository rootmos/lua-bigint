go() {
    make -C tests && ./run -e sandbox
    # lua go.lua
}
