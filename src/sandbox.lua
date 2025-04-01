local P = require"polynomial"

local M = require("arbbase")
local Ascii = require("ascii")

print(Ascii.le_digits_to_be_string(M.to_hex(Ascii.be_string_to_le_digits("180"))))
