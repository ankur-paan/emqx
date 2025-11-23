%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_storage_schema).

-include_lib("typerefl/include/types.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-export([namespace/0, roots/0, fields/1, desc/1]).

-include("../include/emqx_redis_storage.hrl").

%%--------------------------------------------------------------------
%% Hocon Schema
%%--------------------------------------------------------------------

namespace() -> "redis_storage".

roots() ->
    [
        {redis_storage, #{
            type => hoconsc:ref(?MODULE, redis_storage),
            desc => "Redis storage backend configuration"
        }}
    ].

fields(redis_storage) ->
    [
        {enable, #{
            type => boolean(),
            default => false,
            desc => "Enable Redis storage backend"
        }},
        {redis, #{
            type => hoconsc:ref(?MODULE, redis),
            desc => "Redis connection configuration"
        }},
        {persistence, #{
            type => hoconsc:ref(?MODULE, persistence),
            desc => "Persistence settings"
        }}
    ];

fields(redis) ->
    [
        {type, #{
            type => hoconsc:enum([single, sentinel, cluster]),
            default => single,
            desc => "Redis deployment type"
        }},
        {servers, #{
            type => string(),
            default => "127.0.0.1:6379",
            desc => "Comma-separated list of Redis servers (host:port)"
        }},
        {sentinel, #{
            type => string(),
            required => false,
            desc => "Sentinel master name (required for sentinel mode)"
        }},
        {pool_size, #{
            type => pos_integer(),
            default => ?DEFAULT_POOL_SIZE,
            desc => "Connection pool size"
        }},
        {database, #{
            type => non_neg_integer(),
            default => ?DEFAULT_DATABASE,
            desc => "Redis database number (not applicable for cluster mode)"
        }},
        {password, #{
            type => string(),
            required => false,
            sensitive => true,
            desc => "Redis password"
        }},
        {connect_timeout, #{
            type => pos_integer(),
            default => ?DEFAULT_CONNECT_TIMEOUT,
            desc => "Connection timeout in milliseconds"
        }},
        {ssl, #{
            type => hoconsc:ref(?MODULE, ssl),
            desc => "SSL/TLS configuration"
        }}
    ];

fields(ssl) ->
    [
        {enable, #{
            type => boolean(),
            default => false,
            desc => "Enable SSL/TLS"
        }},
        {cacertfile, #{
            type => string(),
            required => false,
            desc => "Path to CA certificate file"
        }},
        {certfile, #{
            type => string(),
            required => false,
            desc => "Path to client certificate file"
        }},
        {keyfile, #{
            type => string(),
            required => false,
            desc => "Path to client key file"
        }}
    ];

fields(persistence) ->
    [
        {session_ttl, #{
            type => pos_integer(),
            default => 86400,
            desc => "Session TTL in Redis (seconds)"
        }},
        {message_ttl, #{
            type => pos_integer(),
            default => 86400,
            desc => "Message TTL in Redis before archive (seconds)"
        }},
        {rdb_enabled, #{
            type => boolean(),
            default => true,
            desc => "Enable RDB snapshots (configured on Redis server)"
        }},
        {aof_enabled, #{
            type => boolean(),
            default => true,
            desc => "Enable AOF (configured on Redis server)"
        }}
    ].

desc(redis_storage) ->
    "Redis storage backend for EMQX sessions and messages";
desc(redis) ->
    "Redis connection configuration";
desc(ssl) ->
    "SSL/TLS configuration for Redis connection";
desc(persistence) ->
    "Persistence settings for Redis storage";
desc(_) ->
    undefined.
