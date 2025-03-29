local M = {
    alphabeth = "0123456789abcdefghijklmnopqrstuvwxyz",
}

function M.be_string_to_le_digits(str)
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

function M.le_digits_to_be_string(p)
    local s = ""
    for i = (p.n or #p),1,-1 do
        local k = p[i]
        local d = alphabeth:sub(k+1, k+1)
        if d == "" then
            error(string.format("unable to encode digit: %d (at %d)", k, i))
        end
        s = s .. d
    end
    return s
end

return M
