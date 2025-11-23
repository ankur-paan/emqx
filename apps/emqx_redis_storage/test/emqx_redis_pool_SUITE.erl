%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_pool_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() -> 
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    case is_redis_available() of
        true ->
            Config;
        false ->
            {skip, no_redis}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

t_start_stop(_Config) ->
    %% Test basic pool start/stop
    Config = #{
        pool_size => 8,
        redis_type => single,
        servers => list_to_binary(?REDIS_HOST ++ ":" ++ integer_to_list(?REDIS_PORT)),
        database => 0
    },
    
    {ok, Pid} = emqx_redis_pool:start_link(Config),
    ?assert(is_process_alive(Pid)),
    
    ok = emqx_redis_pool:stop(),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

t_query(_Config) ->
    %% Test basic Redis query
    Config = #{
        pool_size => 8,
        redis_type => single,
        servers => list_to_binary(?REDIS_HOST ++ ":" ++ integer_to_list(?REDIS_PORT)),
        database => 0
    },
    
    {ok, _Pid} = emqx_redis_pool:start_link(Config),
    
    %% Note: This test would require actual Redis integration
    %% For now, we just test that the pool starts
    
    ok = emqx_redis_pool:stop().

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

is_redis_available() ->
    case gen_tcp:connect(?REDIS_HOST, ?REDIS_PORT, [], 1000) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            true;
        {error, _} ->
            false
    end.
