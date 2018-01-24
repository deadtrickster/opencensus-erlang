-module(stat_prometheus).

-export([run/0]).

run() ->
  ocp:start_trace(),

  %% second argument is exporter options.
  oc_stat_exporter:register(oc_stat_prometheus_exporter, [{registry, 'my.org'}]),

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
  oc_stat:record("my.org/measures/video_size_cum", rand:uniform(1 bsl 63)),

  timer:sleep(2000),

  prometheus_text_format:format('my.org').
