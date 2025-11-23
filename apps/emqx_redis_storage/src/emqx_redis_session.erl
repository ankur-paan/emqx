%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_session).

%% API
-export([
    save_session/2,
    load_session/1,
    delete_session/1,
    list_sessions/2,
    count_sessions/0
]).

-include("../include/emqx_redis_storage.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Store session in Redis hash
-spec save_session(binary(), term()) -> ok | {error, term()}.
save_session(ClientId, SessionData) ->
    Key = session_key(ClientId),
    Timestamp = erlang:system_time(second),
    Node = atom_to_binary(node(), utf8),
    
    %% Encode session data
    EncodedData = term_to_binary(SessionData),
    
    %% Calculate expiry
    ExpiresAt = Timestamp + ?DEFAULT_SESSION_TTL,
    
    %% Build HMSET command
    Fields = [
        <<"data">>, EncodedData,
        <<"created_at">>, integer_to_binary(Timestamp),
        <<"updated_at">>, integer_to_binary(Timestamp),
        <<"expires_at">>, integer_to_binary(ExpiresAt),
        <<"node">>, Node
    ],
    
    %% Execute commands in pipeline
    Commands = [
        [<<"HMSET">>, Key | Fields],
        [<<"EXPIRE">>, Key, integer_to_binary(?DEFAULT_SESSION_TTL)],
        [<<"ZADD">>, <<?SYNC_SESSIONS_KEY>>, integer_to_binary(Timestamp), ClientId]
    ],
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Retrieve session from Redis
-spec load_session(binary()) -> {ok, term()} | {error, not_found} | {error, term()}.
load_session(ClientId) ->
    Key = session_key(ClientId),
    
    case emqx_redis_pool:query([<<"HGET">>, Key, <<"data">>]) of
        {ok, undefined} ->
            {error, not_found};
        {ok, EncodedData} when is_binary(EncodedData) ->
            try
                SessionData = binary_to_term(EncodedData),
                {ok, SessionData}
            catch
                error:Reason ->
                    {error, {decode_error, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Remove session from Redis
-spec delete_session(binary()) -> ok | {error, term()}.
delete_session(ClientId) ->
    Key = session_key(ClientId),
    
    Commands = [
        [<<"DEL">>, Key],
        [<<"ZREM">>, <<?SYNC_SESSIONS_KEY>>, ClientId]
    ],
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc List sessions with pagination
-spec list_sessions(non_neg_integer(), non_neg_integer()) -> {ok, [binary()]} | {error, term()}.
list_sessions(Limit, Offset) ->
    %% Use ZSCAN or ZRANGE for pagination
    Start = Offset,
    Stop = Offset + Limit - 1,
    
    case emqx_redis_pool:query([<<"ZRANGE">>, <<?SYNC_SESSIONS_KEY>>, 
                                 integer_to_binary(Start), 
                                 integer_to_binary(Stop)]) of
        {ok, ClientIds} -> {ok, ClientIds};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Count total active sessions
-spec count_sessions() -> {ok, non_neg_integer()} | {error, term()}.
count_sessions() ->
    case emqx_redis_pool:query([<<"ZCARD">>, <<?SYNC_SESSIONS_KEY>>]) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {ok, Count} when is_binary(Count) -> {ok, binary_to_integer(Count)};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

session_key(ClientId) ->
    <<?SESSION_PREFIX, ClientId/binary>>.

execute_pipeline(Commands) ->
    %% Execute multiple commands
    %% For now, execute them sequentially
    %% TODO: Use Redis pipeline for better performance
    Results = lists:map(
        fun(Cmd) ->
            emqx_redis_pool:query(Cmd)
        end,
        Commands
    ),
    
    %% Check if all succeeded
    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true -> {ok, Results};
        false ->
            %% Find first error
            case lists:dropwhile(fun({ok, _}) -> true; (_) -> false end, Results) of
                [{error, Reason} | _] -> {error, Reason};
                [] -> {error, unknown_error}
            end
    end.
