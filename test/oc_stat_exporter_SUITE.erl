-module(oc_stat_exporter_SUITE).

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
                               exporter_registration,
                               exporter_conf_registration,
                               exporter_invocation%% ,
                               %% full
                              ]}
    ].

init_per_suite(Config) ->
    application:load(opencensus),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(exporter_conf_registration, Config) ->
    application:set_env(opencensus, stat, [{exporters, [{?MODULE, []}]}]),
    {ok, _} = application:ensure_all_started(opencensus),
    Config;
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

exporter_registration(_Config) ->
    oc_stat_exporter:register(?MODULE),
    ?assertMatch(true, oc_stat_exporter:registered(?MODULE)),
    ?assertMatch(false, oc_stat_exporter:registered(qwe)).

exporter_conf_registration(_Config) ->
    ?assertMatch(true, oc_stat_exporter:registered(?MODULE)),
    ?assertMatch(false, oc_stat_exporter:registered(qwe)).

exporter_invocation(_Config) ->
    oc_stat_exporter:register(?MODULE, self()),
    oc_stat_exporter:export([1]),
    ?assertMatch([1], receive
                          Thing -> Thing
                      after 1000 ->
                              nothing
                      end).

full(_Config) ->
    ocp:start_trace(),

    Tid = ets:new(ets_exporter_table, [set, private]),

    %% second argument is exporter options.
    oc_stat_exporter:register(ets_exporter, [{table, Tid}]),

    %% we use concrete module to declare(idempotent) measure, but just oc_stat:record to record.
    %% should we accept strings, binaries, and atoms? or only atoms? comparing atoms is obviously faster
    ok = oc_stat_measure_int64:declare("my.org/measures/video_count", "number of processed videos", ""),
    ok = oc_stat_measure_int64:declare("my.org/measures/video_size_cum", "size of processed video", "Mb"),

    %% little helper that declares the view and subscribes it in one call.
    ok = oc_stat_view:declare_and_subscribe(
           "video_count",
           "number of videos processed processed over time",
           [#{tag => value}], %% proplist/map of static tags, I want to add dynamic tags as an extension
           "my.org/measures/video_count",
           oc_stat_count_aggregation, %% or just count?
           oc_stat_cumulative), %% or just cumulative?

    ok = oc_stat_view:declare_and_subscribe(
           "video_size",
           "number of videos processed processed over time",
           [#{tag => value}],
           "my.org/measures/video_size_cum",
           {oc_stat_distribution_aggregation, {0, 1 bsl 16, 1 bsl 32}} , %% or just distribution?
           oc_stat_cumulative), %% or just cumulative?

    %% how often reported called
    oc_stat:set_reporting_period(1000),

    oc_stat:record("my.org/measures/video_count", 1), %% assume ?CONTEXT
    Size = rand:uniform(1 bsl 63),
    oc_stat:record("my.org/measures/video_size_cum", Size),

    timer:sleep(2000),

    ?assertMatch([{video_count, #{#{tag => value} := 1}},
                  {video_count, #{#{tag => value} := Size}}], ets:tab2list()).
