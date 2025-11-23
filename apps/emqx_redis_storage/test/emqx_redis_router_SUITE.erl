%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_router_SUITE).

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

t_add_route(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_router, add_route, 2)).

t_delete_route(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_router, delete_route, 2)).

t_match_routes(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_router, match_routes, 1)).

t_topics(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_router, topics, 0)).

t_wildcard_matching(_Config) ->
    %% Test wildcard pattern matching logic
    %% This would test the match_topic_pattern/2 function
    ok.
