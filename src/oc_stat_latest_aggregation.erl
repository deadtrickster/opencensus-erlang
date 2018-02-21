-module(oc_stat_latest_aggregation).

-export([init/4,
         add_sample/4,
         export/2]).

-export(['__init_backend__'/0]).

init(_Name, _Description, _Keys, Options) ->
    Options.

add_sample(View, Tags, Value, _Options) ->
    case ets:update_element(?MODULE, {View, Tags}, {2, Value}) of
        false -> ets:insert(?MODULE, {{View, Tags}, Value});
        _ -> Value
    end.

export(View, _Options) ->
    ets:match_object(?MODULE, {{View, '_'}, '_'}).

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {write_concurrency, true}]),
    ok.
