%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_message).

%% API
-export([
    store_message/2,
    get_messages/3,
    ack_message/2,
    delete_messages/1
]).

-include("../include/emqx_redis_storage.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Store inflight message
-spec store_message(binary(), term()) -> ok | {error, term()}.
store_message(ClientId, Message) ->
    %% Extract message ID and timestamp
    MessageId = extract_message_id(Message),
    Timestamp = extract_timestamp(Message),
    
    %% Encode message
    EncodedMessage = term_to_binary(Message),
    
    %% Build keys
    MsgKey = message_key(ClientId, MessageId),
    QueueKey = queue_key(ClientId),
    SyncKey = ?SYNC_MESSAGES_KEY,
    
    %% Execute commands
    Commands = [
        [<<"SET">>, MsgKey, EncodedMessage],
        [<<"EXPIRE">>, MsgKey, integer_to_binary(?DEFAULT_MESSAGE_TTL)],
        [<<"ZADD">>, QueueKey, integer_to_binary(Timestamp), MessageId],
        [<<"ZADD">>, SyncKey, integer_to_binary(Timestamp), MsgKey]
    ],
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Retrieve client messages with pagination
-spec get_messages(binary(), non_neg_integer(), non_neg_integer()) -> 
    {ok, [term()]} | {error, term()}.
get_messages(ClientId, Offset, Limit) ->
    QueueKey = queue_key(ClientId),
    
    %% Get message IDs from sorted set
    Start = Offset,
    Stop = Offset + Limit - 1,
    
    case emqx_redis_pool:query([<<"ZRANGE">>, QueueKey, 
                                 integer_to_binary(Start), 
                                 integer_to_binary(Stop)]) of
        {ok, MessageIds} ->
            %% Fetch actual messages
            fetch_messages(ClientId, MessageIds);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Acknowledge message delivery
-spec ack_message(binary(), binary()) -> ok | {error, term()}.
ack_message(ClientId, MessageId) ->
    MsgKey = message_key(ClientId, MessageId),
    QueueKey = queue_key(ClientId),
    SyncKey = ?SYNC_MESSAGES_KEY,
    
    Commands = [
        [<<"DEL">>, MsgKey],
        [<<"ZREM">>, QueueKey, MessageId],
        [<<"ZREM">>, SyncKey, MsgKey]
    ],
    
    case execute_pipeline(Commands) of
        {ok, _Results} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Clear client message queue
-spec delete_messages(binary()) -> ok | {error, term()}.
delete_messages(ClientId) ->
    QueueKey = queue_key(ClientId),
    
    %% Get all message IDs
    case emqx_redis_pool:query([<<"ZRANGE">>, QueueKey, <<"0">>, <<"-1">>]) of
        {ok, MessageIds} ->
            %% Delete all messages and queue
            MsgKeys = [message_key(ClientId, MsgId) || MsgId <- MessageIds],
            SyncKey = ?SYNC_MESSAGES_KEY,
            
            Commands = 
                [[<<"DEL">>, MsgKey] || MsgKey <- MsgKeys] ++
                [[<<"ZREM">>, SyncKey] ++ MsgKeys] ++
                [[<<"DEL">>, QueueKey]],
            
            case execute_pipeline(Commands) of
                {ok, _Results} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

message_key(ClientId, MessageId) ->
    <<?MSG_PREFIX, ClientId/binary, ":", MessageId/binary>>.

queue_key(ClientId) ->
    <<?MSGQ_PREFIX, ClientId/binary>>.

extract_message_id(Message) when is_map(Message) ->
    maps:get(id, Message, generate_message_id());
extract_message_id(_) ->
    generate_message_id().

extract_timestamp(Message) when is_map(Message) ->
    maps:get(timestamp, Message, erlang:system_time(second));
extract_timestamp(_) ->
    erlang:system_time(second).

generate_message_id() ->
    %% Generate a unique message ID
    Timestamp = erlang:system_time(microsecond),
    Rand = rand:uniform(1000000),
    list_to_binary(io_lib:format("~p-~p", [Timestamp, Rand])).

fetch_messages(_ClientId, []) ->
    {ok, []};
fetch_messages(ClientId, MessageIds) ->
    MsgKeys = [message_key(ClientId, MsgId) || MsgId <- MessageIds],
    
    %% Use MGET to fetch multiple messages
    case emqx_redis_pool:query([<<"MGET">> | MsgKeys]) of
        {ok, EncodedMessages} ->
            Messages = lists:filtermap(
                fun(undefined) -> false;
                   (EncodedMsg) ->
                        try
                            {true, binary_to_term(EncodedMsg)}
                        catch
                            error:_ -> false
                        end
                end,
                EncodedMessages
            ),
            {ok, Messages};
        {error, Reason} ->
            {error, Reason}
    end.

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
