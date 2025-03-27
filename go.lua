local function be_string_to_le_digits_table(str)
    local xs = {}
    local n = #str
    for i = 1,n do
        local j = n - i + 1
        local k = str:byte(j)
        if 48 <= k and k <= 57 then
            xs[i] = k - 48
        elseif 97 <= k and k <= 122 then
            xs[i] = k - 87
        elseif 65 <= k and k <= 90 then
            xs[i] = k - 55
        else
            error(string.format("unable to decode character: %s (at %d)", string.char(k), j))
        end
    end
    return xs
end

print(table.unpack(be_string_to_le_digits_table("0123456789abcdefghijklmnOpqrsTuvWxyz")))
print(table.unpack(be_string_to_le_digits_table("123456890")))
