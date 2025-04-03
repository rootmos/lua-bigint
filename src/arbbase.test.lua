local lu = require("luaunit")
local M = require("arbbase")
local Ascii = require("ascii")

local function run_dec_to_hex(a0, b0)
  local b1 = M.dec_to_hex(Ascii.be_string_to_le_digits(a0))
  local a1 = Ascii.le_digits_to_be_string(b1)
  lu.assertEquals(a1, b0)
end

local function run_hex_to_dec(a0, b0)
  local b1 = M.hex_to_dec(Ascii.be_string_to_le_digits(a0))
  local a1 = Ascii.le_digits_to_be_string(b1)
  lu.assertEquals(a1, b0)
end

local function run_example(dec, hex)
  run_dec_to_hex(dec, hex)
  run_hex_to_dec(hex, dec)
end

function test_example0()
  run_example(
    "10000000000000000000000000000000000000000000000000000000000000000000000000000000001",
    "15159af8044462379881065d41ad19c19af0eeb576360c36400000000000000000001"
  )
end

local mk_a_big_number_up_my_sleave = function()
  local s = ""
  s = s .. "446389124460659487126183309825"
  s = s .. "487850137110474299451140334030"
  s = s .. "147443602279185006217521327965"
  s = s .. "161678919197184770807854966380"
  s = s .. "562358317699194347608500625616"
  s = s .. "179996288658701340728253787732"
  s = s .. "269344923980659338166100763697"
  s = s .. "604252450250972946312255985533"
  s = s .. "770190449954787573727141103072"
  s = s .. "718452556135754670551208126740"
  s = s .. "909518149588531105974683579244"
  s = s .. "901647663863952668286928187049"
  s = s .. "930475596362219050138231189939"
  s = s .. "124307724290313610365014327096"
  s = s .. "158292531077120671747929375994"
  s = s .. "921599569614143225201150504097"
  s = s .. "603342648346183302315005208604"
  s = s .. "846647656241269709794620262632"
  s = s .. "464337848156959150272820928596"
  return s
end

function test_example1()
  run_example(
    mk_a_big_number_up_my_sleave(),
    "142ffdb89886eaa285d18e52687a39b993ec2eaad4c4b1e57b8d2e8a4e91242e6030f22e3710921d04a9013af8e4a5ea76e6a2c3aae983417bc8ed44ca3e890fad50f537dc39a729e8bc02b429d72df837ae180d5779a26c9974f52c1316b1dbe5ee40475edec045a96bce696429281e9bd11d7ab13474c40f76d1b5f0fdfd0fb28572f5a31d07a80aeae2a8eca814b2b341639439dc1145a79d0ca4c7c5fcb46c16c68e73f9f11c60b4d14d477bb549ba2d3064db38227a182781b8c958fa049eb8cc1dc651e89c985fd2dc0404c0b62aa0e5de7de3aebcca28276d45f5f613e4a60c9b846dbc5c8245a5e054"
  )
end

os.exit(lu.LuaUnit.run())
