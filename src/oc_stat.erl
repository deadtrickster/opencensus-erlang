-module(oc_stat).

-export([record/3,
         export/0]).
record(Measure, ContextTags, Value) ->
    [oc_stat_view:add_sample(View, ContextTags, Value)
     || View <- oc_stat_view:measure_views(Measure),
        oc_stat_view:subscribed(View)].

export() ->
    [oc_stat_view:export(View) || View <- oc_stat_view:subscribed()].
