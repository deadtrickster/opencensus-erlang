-module(oc_stat_cumulative).

-export([add_sample/4,
         export/2]).

add_sample(View, Tags, Aggregation, Value) ->
    {AggregationModule, AggregationOptions} = Aggregation,
    AggregationModule:add_sample(View, Tags, Value, AggregationOptions).

export(View, Aggregation) ->
    {AggregationModule, AggregationOptions} = Aggregation,
    AggregationModule:export(View, AggregationOptions).
