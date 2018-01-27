-module(oc_stat_measure_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opencensus.hrl").

all() ->
    [
     {group, positive}
    ].

groups() ->
    [
     {positive, [sequential], [
                               measure_registration,
                               measure_deletion
                              ]}
    ].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Name, Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(opencensus),
    ok.

%% ===================================================================
%% Exporter
%% ===================================================================

export(Data, Pid) ->
    Pid ! Data.

%% ===================================================================
%% TESTS
%% ===================================================================

measure_registration(_Config) ->
    oc_stat_measure:new(?MODULE, "Description"),
    ?assertMatch(#measure{name=?MODULE,
                          description="Description",
                          unit=""}, oc_stat_measure:find(?MODULE)),
    oc_stat_measure:new(?MODULE, "Description", ""),
    oc_stat_measure:batch_new([{?MODULE, "Description", ""}]),
    ?assertError(already_exists, oc_stat_measure:new(?MODULE, "Description", "bytes")),
    ?assertMatch(false, oc_stat_measure:find(qwe)).

measure_deletion(_Config) ->
    oc_stat_measure:new(?MODULE, "Description"),
    ?assertMatch(#measure{}, oc_stat_measure:find(?MODULE)),
    oc_stat_measure:delete(?MODULE),
    ?assertMatch(false, oc_stat_measure:find(?MODULE)).
