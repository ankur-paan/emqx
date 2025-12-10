%%--------------------------------------------------------------------
%% Copyright (c) 2025 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_bridge_v2_api_export_import_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("emqx/include/emqx_config.hrl").

all() ->
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Apps = emqx_cth_suite:start(
        [
            emqx,
            emqx_conf,
            emqx_management,
            emqx_bridge,
            emqx_connector
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

t_export_actions(_Config) ->
    %% Export actions (even if empty)
    {200, ExportResult} = call_actions_export_api(post, #{}),
    ?assertMatch(#{filename := _, size := _, created_at := _}, ExportResult),
    Filename = maps:get(filename, ExportResult),
    ?assert(is_binary(Filename)),

    %% Verify the export file exists
    Files = emqx_mgmt_data_backup:list_files(),
    ?assert(lists:any(fun(#{filename := F}) -> F =:= Filename end, Files)),

    ok.

t_export_sources(_Config) ->
    %% Export sources (even if empty)
    {200, ExportResult} = call_sources_export_api(post, #{}),
    ?assertMatch(#{filename := _, size := _, created_at := _}, ExportResult),
    Filename = maps:get(filename, ExportResult),
    ?assert(is_binary(Filename)),

    %% Verify the export file exists
    Files = emqx_mgmt_data_backup:list_files(),
    ?assert(lists:any(fun(#{filename := F}) -> F =:= Filename end, Files)),

    ok.

t_upload_and_import_actions(_Config) ->
    %% Export actions
    {200, #{filename := Filename}} = call_actions_export_api(post, #{}),

    %% Read the exported file
    {ok, FileContent} = emqx_mgmt_data_backup:read_file(Filename),

    %% Upload the file (this validates it)
    {204} = call_actions_upload_api(post, #{
        body => #{
            <<"filename">> => #{
                type => <<"multipart/form-data">>,
                Filename => FileContent
            }
        }
    }),

    %% Import the actions
    {204} = call_actions_import_api(post, #{body => #{<<"filename">> => Filename}}),

    ok.

t_upload_and_import_sources(_Config) ->
    %% Export sources
    {200, #{filename := Filename}} = call_sources_export_api(post, #{}),

    %% Read the exported file
    {ok, FileContent} = emqx_mgmt_data_backup:read_file(Filename),

    %% Upload the file (this validates it)
    {204} = call_sources_upload_api(post, #{
        body => #{
            <<"filename">> => #{
                type => <<"multipart/form-data">>,
                Filename => FileContent
            }
        }
    }),

    %% Import the sources
    {204} = call_sources_import_api(post, #{body => #{<<"filename">> => Filename}}),

    ok.

t_upload_invalid_actions_file(_Config) ->
    %% Try to upload an invalid file for actions
    InvalidContent = <<"This is not a valid backup file">>,
    Filename = <<"invalid-actions.tar.gz">>,

    Result = call_actions_upload_api(post, #{
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

t_import_nonexistent_actions_file(_Config) ->
    %% Try to import a file that doesn't exist
    Result = call_actions_import_api(post, #{body => #{<<"filename">> => <<"nonexistent.tar.gz">>}}),

    %% Should get an error
    ?assertMatch({400, #{code := 'BAD_REQUEST'}}, Result),

    ok.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

call_actions_export_api(Method, Request) ->
    emqx_bridge_v2_api:'/actions/export'(Method, Request).

call_actions_upload_api(Method, Request) ->
    emqx_bridge_v2_api:'/actions/import/upload'(Method, Request).

call_actions_import_api(Method, Request) ->
    emqx_bridge_v2_api:'/actions/import'(Method, Request).

call_sources_export_api(Method, Request) ->
    emqx_bridge_v2_api:'/sources/export'(Method, Request).

call_sources_upload_api(Method, Request) ->
    emqx_bridge_v2_api:'/sources/import/upload'(Method, Request).

call_sources_import_api(Method, Request) ->
    emqx_bridge_v2_api:'/sources/import'(Method, Request).
