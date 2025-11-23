# EMQX Redis Storage Backend

This application provides a Redis-based storage backend for EMQX to enable stateless EMQX nodes that can be replaced without data loss.

## Features

- **Session Storage**: Store MQTT session state in Redis
- **Message Persistence**: Persist QoS 1/2 messages for delivery
- **Distributed Routing**: Shared routing table across EMQX cluster nodes
- **High Availability**: Support for Redis Sentinel and Cluster modes
- **Wildcard Routing**: Efficient MQTT topic wildcard matching using Lua scripts

## Architecture

### Modules

- `emqx_redis_pool`: Connection pool manager using ecpool
- `emqx_redis_session`: Session storage operations
- `emqx_redis_message`: Message persistence for QoS 1/2
- `emqx_redis_router`: Distributed routing table with wildcard support
- `emqx_redis_storage_schema`: HOCON configuration schema

### Redis Data Structures

#### Sessions
```
Key: "session:{client_id}"
Type: Hash
Fields: data, created_at, updated_at, expires_at, node
TTL: Configurable (default 24h)

Index: "sync:sessions" (Sorted Set by timestamp)
```

#### Messages
```
Key: "msg:{client_id}:{message_id}"
Type: String (binary encoded)
TTL: Configurable (default 24h)

Queue: "msgq:{client_id}" (Sorted Set by publish timestamp)
Index: "sync:messages" (Sorted Set by timestamp)
```

#### Routes
```
Exact: "route:{topic}" (Set of nodes)
Wildcards: "route:wildcards" (Sorted Set of "pattern|node")
Index: "sync:routes" (Sorted Set by timestamp)
```

## Configuration

```hocon
redis_storage {
    enable = true
    
    redis {
        type = single  # single | sentinel | cluster
        servers = "127.0.0.1:6379"
        pool_size = 64
        database = 0
        password = "your_password"
        connect_timeout = "5s"
        
        ssl {
            enable = false
            cacertfile = "/path/to/ca.pem"
            certfile = "/path/to/cert.pem"
            keyfile = "/path/to/key.pem"
        }
    }
    
    persistence {
        session_ttl = "24h"
        message_ttl = "24h"
        rdb_enabled = true
        aof_enabled = true
    }
}
```

## Usage

### Start the Application

```erlang
emqx_redis_storage:start().
```

### Session Operations

```erlang
%% Save a session
ClientId = <<"client123">>,
SessionData = #{client_id => ClientId, ...},
emqx_redis_session:save_session(ClientId, SessionData).

%% Load a session
{ok, SessionData} = emqx_redis_session:load_session(ClientId).

%% Delete a session
emqx_redis_session:delete_session(ClientId).

%% List sessions
{ok, ClientIds} = emqx_redis_session:list_sessions(100, 0).

%% Count sessions
{ok, Count} = emqx_redis_session:count_sessions().
```

### Message Operations

```erlang
%% Store a message
Message = #{id => <<"msg1">>, payload => <<"data">>, ...},
emqx_redis_message:store_message(ClientId, Message).

%% Get messages
{ok, Messages} = emqx_redis_message:get_messages(ClientId, 0, 10).

%% Acknowledge message
emqx_redis_message:ack_message(ClientId, MessageId).

%% Delete all messages for client
emqx_redis_message:delete_messages(ClientId).
```

### Routing Operations

```erlang
%% Add a route
Topic = <<"sensors/+/temperature">>,
emqx_redis_router:add_route(Topic, node()).

%% Match routes
{ok, Nodes} = emqx_redis_router:match_routes(<<"sensors/1/temperature">>).

%% Delete a route
emqx_redis_router:delete_route(Topic, node()).

%% List all topics
{ok, Topics} = emqx_redis_router:topics().
```

## Testing

```bash
# Run all tests
make ct

# Run specific test suite
make ct SUITES=apps/emqx_redis_storage
```

## Performance

- Session operations: ~1-2ms
- Message operations: ~1-2ms
- Route matching: ~2-5ms (with Lua script optimization)

## Requirements

- Redis 5.0+
- Erlang/OTP 27+
- EMQX 6.0+

## License

Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
