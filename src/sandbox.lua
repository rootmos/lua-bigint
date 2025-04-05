local P = require"polynomial"
local A = require"arbbase"

--print(A.stencil(10, 16))
--print(A.stencil(16, 10))

-- A < B, always A^n = r + q*B?
print(A.stencil(5, 1024))

print(A.stencil(19, 3))

print(A.stencil(22, 3))
