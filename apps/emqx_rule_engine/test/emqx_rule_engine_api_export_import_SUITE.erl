%%--------------------------------------------------------------------
%% Copyright (c) 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_rule_engine_api_export_import_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("emqx/include/emqx_config.hrl").

-define(SIMPLE_RULE, #{
    <<"description">> => <<"A simple rule for export/import test">>,
    <<"enable">> => true,
    <<"actions">> => [#{<<"function">> => <<"console">>}],
    <<"sql">> => <<"SELECT * from \"t/export_import\"">>,
    <<"name">> => <<"test_export_import_rule">>
}).

all() ->
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Apps = emqx_cth_suite:start(
        [
            emqx,
            emqx_conf,
            emqx_management,
            emqx_rule_engine
        ],
        #{work_dir => emqx_cth_suite:work_dir(Config)}
    ),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = ?config(apps, Config),
    emqx_cth_suite:stop(Apps),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up any created rules
    {200, #{data := Rules}} = call_rules_api(get, #{query_string => #{}}),
    lists:foreach(
        fun(#{id := Id}) ->
            call_rule_api_by_id(delete, Id, #{bindings => #{id => Id}})
        end,
        Rules
    ),
    %% Clean up any exported files
    case emqx_mgmt_data_backup:backup_files() of
        [] -> ok;
        Files ->
            lists:foreach(
                fun(File) ->
                    Filename = filename:basename(File),
                    emqx_mgmt_data_backup:delete_file(Filename)
                end,
                Files
            )
    end,
    ok.

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

t_export_rules(_Config) ->
    %% Create a rule first
    RuleId = <<"export_test_rule">>,
    Rule = ?SIMPLE_RULE#{<<"id">> => RuleId},
    {201, CreatedRule} = call_rules_api(post, #{body => Rule}),
    ?assertEqual(RuleId, maps:get(id, CreatedRule)),

    %% Export rules
    {200, ExportResult} = call_export_api(post, #{}),
    ?assertMatch(#{filename := _, size := _, created_at := _}, ExportResult),
    Filename = maps:get(filename, ExportResult),
    ?assert(is_binary(Filename)),

    %% Verify the export file exists
    Files = emqx_mgmt_data_backup:list_files(),
    ?assert(lists:any(fun(#{filename := F}) -> F =:= Filename end, Files)),

    ok.

t_upload_and_import_rules(_Config) ->
    %% Create a rule to export
    RuleId = <<"upload_import_test_rule">>,
    Rule = ?SIMPLE_RULE#{<<"id">> => RuleId},
    {201, _CreatedRule} = call_rules_api(post, #{body => Rule}),

    %% Export rules
    {200, #{filename := Filename}} = call_export_api(post, #{}),

    %% Read the exported file
    {ok, FileContent} = emqx_mgmt_data_backup:read_file(Filename),

    %% Delete the rule
    {204} = call_rule_api_by_id(delete, RuleId, #{bindings => #{id => RuleId}}),

    %% Verify rule is deleted
    {404, _} = call_rule_api_by_id(get, RuleId, #{bindings => #{id => RuleId}}),

    %% Upload the file (this validates it)
    {204} = call_upload_api(post, #{
        body => #{
            <<"filename">> => #{
                type => <<"multipart/form-data">>,
                Filename => FileContent
            }
        }
    }),

    %% Import the rules
    {204} = call_import_api(post, #{body => #{<<"filename">> => Filename}}),

    %% Verify the rule is restored
    {200, RestoredRule} = call_rule_api_by_id(get, RuleId, #{bindings => #{id => RuleId}}),
    ?assertEqual(RuleId, maps:get(id, RestoredRule)),

    ok.

t_upload_invalid_file(_Config) ->
    %% Try to upload an invalid file
    InvalidContent = <<"This is not a valid backup file">>,
    Filename = <<"invalid.tar.gz">>,

    Result = call_upload_api(post, #{
        body => #{
            <<"filename">> => #{
                type => <<"multipart/form-data">>,
                Filename => InvalidContent
            }
        }
    }),

    %% Should get a 400 error
    ?assertMatch({400, #{code := 'BAD_REQUEST'}}, Result),

    ok.

t_import_nonexistent_file(_Config) ->
    %% Try to import a file that doesn't exist
    Result = call_import_api(post, #{body => #{<<"filename">> => <<"nonexistent.tar.gz">>}}),

    %% Should get an error
    ?assertMatch({400, #{code := 'BAD_REQUEST'}}, Result),

    ok.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

call_rules_api(Method, Request) ->
    emqx_rule_engine_api:'/rules'(Method, Request).

call_rule_api_by_id(Method, _Id, Request) ->
    emqx_rule_engine_api:'/rules/:id'(Method, Request).

call_export_api(Method, Request) ->
    emqx_rule_engine_api:'/rule_engine/export'(Method, Request).

call_upload_api(Method, Request) ->
    emqx_rule_engine_api:'/rule_engine/import/upload'(Method, Request).

call_import_api(Method, Request) ->
    emqx_rule_engine_api:'/rule_engine/import'(Method, Request).
