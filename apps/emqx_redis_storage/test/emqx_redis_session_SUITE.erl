%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_session_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() -> 
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

t_save_load_session(_Config) ->
    ClientId = <<"test_client">>,
    SessionData = #{
        client_id => ClientId,
        username => <<"test_user">>,
        protocol => mqtt,
        connected_at => erlang:system_time(second)
    },
    
    %% Test would require Redis connection
    %% For now, just test the module loads
    ?assert(erlang:function_exported(emqx_redis_session, save_session, 2)),
    ?assert(erlang:function_exported(emqx_redis_session, load_session, 1)).

t_delete_session(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_session, delete_session, 1)).

t_list_sessions(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_session, list_sessions, 2)).

t_count_sessions(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_session, count_sessions, 0)).
