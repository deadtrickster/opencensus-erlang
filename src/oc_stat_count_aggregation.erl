-module(oc_stat_count_aggregation).

-export([add_sample/4,
         export/2]).

-export(['__init_backend__'/0]).

add_sample(View, Tags, _Value, _Options) ->
    ets:update_counter(?MODULE, {View, Tags}, 1, {{View, Tags}, 0}).

export(View, _Options) ->
    ets:match_object(?MODULE, {{View, '_'}, '_'}).

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    ok.
