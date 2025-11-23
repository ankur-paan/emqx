%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-ifndef(EMQX_REDIS_STORAGE_HRL).
-define(EMQX_REDIS_STORAGE_HRL, true).

%% Redis key prefixes
-define(SESSION_PREFIX, <<"session:">>).
-define(MSG_PREFIX, <<"msg:">>).
-define(MSGQ_PREFIX, <<"msgq:">>).
-define(ROUTE_PREFIX, <<"route:">>).
-define(SYNC_SESSIONS_KEY, <<"sync:sessions">>).
-define(SYNC_MESSAGES_KEY, <<"sync:messages">>).
-define(SYNC_ROUTES_KEY, <<"sync:routes">>).
-define(ROUTE_WILDCARDS_KEY, <<"route:wildcards">>).

%% Key separators
-define(KEY_SEPARATOR, <<":">>).

%% Default timeouts (milliseconds)
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_POOL_SIZE, 64).
-define(DEFAULT_DATABASE, 0).
-define(DEFAULT_SESSION_TTL, 86400).  % 24 hours in seconds
-define(DEFAULT_MESSAGE_TTL, 86400).  % 24 hours in seconds

%% Redis types
-define(REDIS_TYPE_SINGLE, single).
-define(REDIS_TYPE_SENTINEL, sentinel).
-define(REDIS_TYPE_CLUSTER, cluster).

-endif.
