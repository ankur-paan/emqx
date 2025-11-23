%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_pool).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    query/1,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("../include/emqx_redis_storage.hrl").

-record(state, {
    resource_id :: binary(),
    config :: map()
}).

-define(RESOURCE_ID, <<"emqx_redis_storage_resource">>).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Execute a Redis command
query(Command) ->
    case emqx_resource:simple_sync_query(?RESOURCE_ID, {cmd, Command}) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Config]) ->
    process_flag(trap_exit, true),
    
    %% Get configuration with defaults
    RedisType = maps:get(redis_type, Config, ?REDIS_TYPE_SINGLE),
    Servers = maps:get(servers, Config, "127.0.0.1:6379"),
    Database = maps:get(database, Config, ?DEFAULT_DATABASE),
    Password = maps:get(password, Config, undefined),
    PoolSize = maps:get(pool_size, Config, ?DEFAULT_POOL_SIZE),
    
    %% Build resource configuration for emqx_redis
    ResourceConfig = build_resource_config(RedisType, Servers, Database, Password, PoolSize, Config),
    
    %% Verify emqx_redis module is available
    case code:ensure_loaded(emqx_redis) of
        {module, emqx_redis} ->
            %% Create Redis resource using emqx_resource
            case emqx_resource:create_local(
                ?RESOURCE_ID,
                <<"emqx_redis_storage">>,
                emqx_redis,
                ResourceConfig,
                #{}
            ) of
                {ok, _} ->
                    {ok, #state{resource_id = ?RESOURCE_ID, config = Config}};
                {error, {already_created, _}} ->
                    %% Resource already exists, that's ok
                    {ok, #state{resource_id = ?RESOURCE_ID, config = Config}};
                {error, Reason} ->
                    {stop, Reason}
            end;
        {error, _} ->
            {stop, {missing_dependency, emqx_redis}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{resource_id = ResourceId}) ->
    _ = emqx_resource:remove_local(ResourceId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

build_resource_config(single, Server, Database, Password, PoolSize, Config) ->
    BaseConfig = #{
        <<"redis_type">> => <<"single">>,
        <<"server">> => ensure_binary(Server),
        <<"database">> => Database,
        <<"auto_reconnect">> => true,
        <<"pool_size">> => PoolSize
    },
    add_optional_fields(BaseConfig, Password, Config);

build_resource_config(sentinel, Servers, Database, Password, PoolSize, Config) ->
    Sentinel = maps:get(sentinel, Config, "mymaster"),
    BaseConfig = #{
        <<"redis_type">> => <<"sentinel">>,
        <<"servers">> => ensure_binary(Servers),
        <<"sentinel">> => ensure_binary(Sentinel),
        <<"database">> => Database,
        <<"auto_reconnect">> => true,
        <<"pool_size">> => PoolSize
    },
    add_optional_fields(BaseConfig, Password, Config);

build_resource_config(cluster, Servers, _Database, Password, PoolSize, Config) ->
    BaseConfig = #{
        <<"redis_type">> => <<"cluster">>,
        <<"servers">> => ensure_binary(Servers),
        <<"auto_reconnect">> => true,
        <<"pool_size">> => PoolSize
    },
    %% Cluster mode doesn't support database selection
    add_optional_fields(BaseConfig, Password, Config).

add_optional_fields(Config, Password, ExtraConfig) ->
    Config1 = case Password of
        undefined -> Config;
        _ -> Config#{<<"password">> => ensure_binary(Password)}
    end,
    
    %% Add SSL if enabled
    Config2 = case maps:get(ssl_enable, ExtraConfig, false) of
        true ->
            SSLConfig = #{
                <<"enable">> => true
            },
            SSLConfig1 = case maps:get(ssl_cacertfile, ExtraConfig, undefined) of
                undefined -> SSLConfig;
                CAFile -> SSLConfig#{<<"cacertfile">> => ensure_binary(CAFile)}
            end,
            SSLConfig2 = case maps:get(ssl_certfile, ExtraConfig, undefined) of
                undefined -> SSLConfig1;
                CertFile -> SSLConfig1#{<<"certfile">> => ensure_binary(CertFile)}
            end,
            SSLConfig3 = case maps:get(ssl_keyfile, ExtraConfig, undefined) of
                undefined -> SSLConfig2;
                KeyFile -> SSLConfig2#{<<"keyfile">> => ensure_binary(KeyFile)}
            end,
            Config1#{<<"ssl">> => SSLConfig3};
        false ->
            Config1#{<<"ssl">> => #{<<"enable">> => false}}
    end,
    
    Config2.

ensure_binary(Val) when is_binary(Val) -> Val;
ensure_binary(Val) when is_list(Val) -> list_to_binary(Val);
ensure_binary(Val) when is_atom(Val) -> atom_to_binary(Val, utf8).
