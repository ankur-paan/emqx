%%--------------------------------------------------------------------
%% Copyright (c) 2020-2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_redis_message_SUITE).

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

t_store_message(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_message, store_message, 2)).

t_get_messages(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_message, get_messages, 3)).

t_ack_message(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_message, ack_message, 2)).

t_delete_messages(_Config) ->
    ?assert(erlang:function_exported(emqx_redis_message, delete_messages, 1)).
