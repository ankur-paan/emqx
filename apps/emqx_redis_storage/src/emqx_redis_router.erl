%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_router).

%% API
-export([
    add_route/2,
    delete_route/2,
    match_routes/1,
    topics/0,
    load_lua_script/0
]).

-include("../include/emqx_redis_storage.hrl").

%% Lua script SHA for wildcard matching
-define(LUA_SCRIPT_KEY, emqx_redis_router_lua_sha).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Add topic route
-spec add_route(binary(), atom()) -> ok | {error, term()}.
add_route(Topic, Node) ->
    NodeBin = atom_to_binary(Node, utf8),
    Timestamp = erlang:system_time(second),
    
    %% Determine if topic has wildcards
    case has_wildcards(Topic) of
        true ->
            %% Store in wildcard index
            Entry = <<Topic/binary, "|", NodeBin/binary>>,
            Commands = [
                [<<"ZADD">>, <<?ROUTE_WILDCARDS_KEY>>, <<"0">>, Entry],
                [<<"ZADD">>, <<?SYNC_ROUTES_KEY>>, integer_to_binary(Timestamp), Topic]
            ];
        false ->
            %% Store in exact match set
            RouteKey = route_key(Topic),
            Commands = [
                [<<"SADD">>, RouteKey, NodeBin],
                [<<"ZADD">>, <<?SYNC_ROUTES_KEY>>, integer_to_binary(Timestamp), Topic]
            ]
    end,
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Remove topic route
-spec delete_route(binary(), atom()) -> ok | {error, term()}.
delete_route(Topic, Node) ->
    NodeBin = atom_to_binary(Node, utf8),
    
    case has_wildcards(Topic) of
        true ->
            Entry = <<Topic/binary, "|", NodeBin/binary>>,
            Commands = [
                [<<"ZREM">>, <<?ROUTE_WILDCARDS_KEY>>, Entry]
            ];
        false ->
            RouteKey = route_key(Topic),
            Commands = [
                [<<"SREM">>, RouteKey, NodeBin]
            ]
    end,
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Find matching routes (with wildcard support)
-spec match_routes(binary()) -> {ok, [atom()]} | {error, term()}.
match_routes(Topic) ->
    %% First, try exact match
    RouteKey = route_key(Topic),
    
    case emqx_redis_pool:query([<<"SMEMBERS">>, RouteKey]) of
        {ok, ExactNodes} ->
            %% Then try wildcard matches using Lua script
            case match_wildcard_routes(Topic) of
                {ok, WildcardNodes} ->
                    AllNodes = lists:usort(ExactNodes ++ WildcardNodes),
                    Atoms = [binary_to_atom(N, utf8) || N <- AllNodes],
                    {ok, Atoms};
                {error, Reason} ->
                    %% Fallback to exact matches only
                    Atoms = [binary_to_atom(N, utf8) || N <- ExactNodes],
                    {ok, Atoms}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List all topics
-spec topics() -> {ok, [binary()]} | {error, term()}.
topics() ->
    case emqx_redis_pool:query([<<"ZRANGE">>, <<?SYNC_ROUTES_KEY>>, <<"0">>, <<"-1">>]) of
        {ok, Topics} -> {ok, Topics};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Load Lua script for wildcard matching
-spec load_lua_script() -> ok | {error, term()}.
load_lua_script() ->
    Script = read_lua_script(),
    
    case emqx_redis_pool:query([<<"SCRIPT">>, <<"LOAD">>, Script]) of
        {ok, SHA} ->
            %% Store SHA for later use
            persistent_term:put(?LUA_SCRIPT_KEY, SHA),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

route_key(Topic) ->
    <<?ROUTE_PREFIX, Topic/binary>>.

has_wildcards(Topic) ->
    binary:match(Topic, [<<"+">>, <<"#">>]) =/= nomatch.

match_wildcard_routes(Topic) ->
    %% Try to use Lua script for efficient wildcard matching
    case persistent_term:get(?LUA_SCRIPT_KEY, undefined) of
        undefined ->
            %% Fallback to Erlang-based matching
            match_wildcards_erlang(Topic);
        SHA ->
            %% Use EVALSHA for cached script
            case emqx_redis_pool:query([<<"EVALSHA">>, SHA, <<"0">>, Topic]) of
                {ok, Nodes} -> {ok, Nodes};
                {error, _} ->
                    %% Script might be evicted, reload and retry
                    case load_lua_script() of
                        ok -> match_wildcard_routes(Topic);
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

match_wildcards_erlang(Topic) ->
    %% Fallback: fetch all wildcard routes and match in Erlang
    case emqx_redis_pool:query([<<"ZRANGE">>, <<?ROUTE_WILDCARDS_KEY>>, <<"0">>, <<"-1">>]) of
        {ok, Entries} ->
            Nodes = lists:filtermap(
                fun(Entry) ->
                    case binary:split(Entry, <<"|">>) of
                        [Pattern, Node] ->
                            case match_topic_pattern(Topic, Pattern) of
                                true -> {true, Node};
                                false -> false
                            end;
                        _ -> false
                    end
                end,
                Entries
            ),
            {ok, Nodes};
        {error, Reason} ->
            {error, Reason}
    end.

match_topic_pattern(Topic, Pattern) ->
    %% Simple wildcard matching
    %% + matches exactly one level
    %% # matches zero or more levels
    TopicLevels = binary:split(Topic, <<"/">>, [global]),
    PatternLevels = binary:split(Pattern, <<"/">>, [global]),
    match_levels(TopicLevels, PatternLevels).

match_levels([], []) ->
    true;
match_levels(_, [<<"#">>]) ->
    true;
match_levels([_TH | TT], [<<"+">> | PT]) ->
    match_levels(TT, PT);
match_levels([TH | TT], [TH | PT]) ->
    match_levels(TT, PT);
match_levels(_, _) ->
    false.

read_lua_script() ->
    %% Read the Lua script from file or inline
    %% For now, inline the script
    <<"
local topic = ARGV[1]
local wildcards = redis.call('ZRANGE', 'route:wildcards', 0, -1)
local matched = {}

for i, entry in ipairs(wildcards) do
    local separator = string.find(entry, '|')
    if separator then
        local pattern = string.sub(entry, 1, separator - 1)
        local node = string.sub(entry, separator + 1)
        
        -- Convert MQTT wildcard to Lua pattern
        local lua_pattern = pattern
        lua_pattern = string.gsub(lua_pattern, '+', '[^/]+')
        lua_pattern = string.gsub(lua_pattern, '#', '.*')
        lua_pattern = '^' .. lua_pattern .. '$'
        
        if string.match(topic, lua_pattern) then
            table.insert(matched, node)
        end
    end
end

return matched
">>.

execute_pipeline(Commands) ->
    Results = lists:map(
        fun(Cmd) ->
            emqx_redis_pool:query(Cmd)
        end,
        Commands
    ),
    
    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true -> {ok, Results};
        false ->
            case lists:dropwhile(fun({ok, _}) -> true; (_) -> false end, Results) of
                [{error, Reason} | _] -> {error, Reason};
                [] -> {error, unknown_error}
            end
    end.
