-- match_wildcard_routes.lua
-- Efficiently match MQTT topic wildcards in Redis

local topic = ARGV[1]
local wildcards = redis.call('ZRANGE', 'route:wildcards', 0, -1)
local matched = {}

for i, entry in ipairs(wildcards) do
    local separator = string.find(entry, '|')
    if separator then
        local pattern = string.sub(entry, 1, separator - 1)
        local node = string.sub(entry, separator + 1)
        
        -- Convert MQTT wildcard to Lua pattern
        -- + matches exactly one level (not /)
        -- # matches zero or more levels (including /)
        local lua_pattern = pattern
        lua_pattern = string.gsub(lua_pattern, '%+', '[^/]+')
        lua_pattern = string.gsub(lua_pattern, '#', '.*')
        lua_pattern = '^' .. lua_pattern .. '$'
        
        if string.match(topic, lua_pattern) then
            table.insert(matched, node)
        end
    end
end

return matched
