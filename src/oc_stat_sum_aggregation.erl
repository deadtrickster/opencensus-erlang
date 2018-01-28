-module(oc_stat_sum_aggregation).

-export([add_sample/4,
         export/2]).

-export(['__init_backend__'/0]).

-define(SUM_POS, 3).
-define(COUNTER_POS, 2).

add_sample(View, Tags, Value, _Options) ->
    ets:update_counter(?MODULE, {View, Tags}, [{?COUNTER_POS, 1}, {?SUM_POS, Value}], {{View, Tags}, 0, 0}).

export(View, _Options) ->
    ets:match_object(?MODULE, {{View, '_'}, '_', '_'}).

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    ok.
