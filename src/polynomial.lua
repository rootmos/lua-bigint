local a = {1, 2, 3, n=0}
local b = {4, 0, 5, n=1, v="y"}

local function tostring(p)
    s = ""
    v = p.v or "x"
    for i, k in ipairs(p) do
        if k ~= 0 then
            if s ~= "" then
                s = s .. " + "
            end

            local n = (p.n or 0) + i - 1

            if n == 0 then
                s = s .. string.format("%d", k)
            elseif n == 1 then
                s = s .. string.format("%d%s", k, v)
            else
                s = s .. string.format("%d%s^%d", k, v, n)
            end
        end
    end

    return s
end

print(tostring(a))
print(tostring(b))
