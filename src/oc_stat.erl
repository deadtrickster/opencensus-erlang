-module(oc_stat).

-export([record/2,
         export/0]).

record(Measure, Value) ->
    %% TODO: do not expose oc_stat_view internals here
    [oc_stat_view:add_sample(Name, Tags, Window, Aggregation, Value)
     || {_Measure, Name, Subscribed, _Description, Tags, Aggregation, Window}
            <- oc_stat_view:measure_views(Measure), Subscribed].

export() ->
    [oc_stat_view:export(View) || View <- oc_stat_view:subscribed()].
