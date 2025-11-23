%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_storage_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("../include/emqx_redis_storage.hrl").

start(_StartType, _StartArgs) ->
    emqx_redis_storage_sup:start_link().

stop(_State) ->
    ok.
