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
                               exporter_invocation,
                               full
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
init_per_testcase(full, Config) ->
    Views = [#{
                name => "video_size",
                description => "number of videos processed processed over time",
                tags => [#{ctag => value}],
                measure => 'my.org/measures/video_size_sum',
                aggregation => {oc_stat_distribution_aggregation, [0, 1 bsl 16, 1 bsl 32]}
              }],

    application:set_env(opencensus, stat, [{views, Views}]),
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

    oc_stat_exporter:register(ets_exporter, [{table, Tid}]),

    ok = oc_stat_view:subscribe(
           "video_count",
           "number of videos processed processed over time",
           [#{ctag => value},
            type],
           'my.org/measures/video_count',
           oc_stat_count_aggregation),

    ok = oc_stat_view:subscribe(
           "video_sum",
           "video_size_sum",
           [#{sum_tag => value},
            type, category],
           'my.org/measures/video_size_sum',
           oc_stat_sum_aggregation),

    ok = oc_stat_view:subscribe(
           "last_video_size",
           "last processed video size",
           [#{ctag => value}],
           'my.org/measures/video_size_sum',
           oc_stat_latest_aggregation),

    %% how often reported called
    %% oc_stat:set_reporting_period(1000),

    oc_stat:record('my.org/measures/video_count', #{type=>"mpeg", category=>"category1"}, 1), %% assume ?CONTEXT
    oc_stat:record('my.org/measures/video_count', #{type=>"mpeg", category=>"category1"}, 1), %% assume ?CONTEXT
    oc_stat:record('my.org/measures/video_size_sum', #{type=>"mpeg", category=>"category1"}, 1024), %% assume ?CONTEXT
    oc_stat:record('my.org/measures/video_size_sum', #{type=>"mpeg", category=>"category1"}, 4096), %% assume ?CONTEXT
    %% Size = rand:uniform(1 bsl 63),
    %% oc_stat:record('my.org/measures/video_size_cum', Size),

    %% timer:sleep(2000),

    ?assertMatch([#{aggregation := {oc_stat_count_aggregation, []},
                    description := "number of videos processed processed over time",
                    name := "video_count",
                    rows := [{{"video_count", ["mpeg"]}, 2}],
                    tags := {#{ctag := value}, [type]}},
                  #{aggregation :=
                        {oc_stat_distribution_aggregation, [0, 65536, 4294967296]},
                    description := "number of videos processed processed over time",
                    name := "video_size",
                    rows := [{{"video_size", []}, 5120, 0, 2, 0}],
                    tags := {#{ctag := value}, []}},
                  #{aggregation := {oc_stat_latest_aggregation,[]},
                    description := "last processed video size",
                    name := "last_video_size",
                    rows := [{{"last_video_size",[]},4096}],
                    tags := {#{ctag := value},[]}},
                  #{aggregation := {oc_stat_sum_aggregation, []},
                    description := "video_size_sum", name := "video_sum",
                    rows := [{{"video_sum", ["category1", "mpeg"]}, 2, 5120}],
                    tags := {#{sum_tag := value}, [category, type]}}],  lists:sort(oc_stat:export()))

    %% ?assertMatch([{video_count, #{#{tag => value} := 1}} %%,
    %%               %% {video_count, #{#{tag => value} := Size}}
    %%              ], ets:tab2list(Tid))
        .
