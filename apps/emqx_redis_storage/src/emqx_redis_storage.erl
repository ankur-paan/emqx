%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_storage).

-export([
    start/0,
    stop/0
]).

-include("../include/emqx_redis_storage.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start() ->
    application:ensure_all_started(emqx_redis_storage).

stop() ->
    application:stop(emqx_redis_storage).
