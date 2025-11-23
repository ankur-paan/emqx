%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_storage_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    
    ChildSpecs = [
        #{
            id => emqx_redis_pool,
            start => {emqx_redis_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [emqx_redis_pool]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
