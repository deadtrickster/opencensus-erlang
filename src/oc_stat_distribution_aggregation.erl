-module(oc_stat_distribution_aggregation).

-export([init/4,
         add_sample/4,
         export/2]).

-export(['__init_backend__'/0]).

-define(SUM_POS, 2).
-define(BUCKETS_POS, 3).

init(_Name, _Description, _Keys, Options) ->
    Options.

%% TODO: add +infinity bound implicitly
add_sample(View, Tags, Value, Buckets) ->
    BucketPosition =
        calculate_histogram_update_positions(Buckets, Value),
    Up = fun() ->
                 ets:update_counter(?MODULE, {View, Tags},
                                    [{?SUM_POS, Value}, {?SUM_POS + BucketPosition, 1}])
         end,
    try
        Up()
    catch error:badarg ->
            Things =
                [{View, Tags}, Value]
                ++ lists:duplicate(BucketPosition - 1, 0)
                ++ [1]
                ++ lists:duplicate(length(Buckets) - BucketPosition, 0),
            case ets:insert_new(?MODULE, list_to_tuple(Things)) of
                false -> Up();
                true -> ok
            end
    end.

export(View, Buckets) ->
    BoundCounters = lists:duplicate(length(Buckets), '_'),
    Match = [{View, '_'}, '_'] ++ BoundCounters,
    ets:match_object(?MODULE, list_to_tuple(Match)).

calculate_histogram_update_positions(Buckets, Value) ->
     position(Buckets, fun(Bound) ->
                               Value =< Bound
                       end).

%% linear search!
position(List, Pred) ->
    position(List, Pred, 1).

position([], _Pred, _Pos) ->
    0;
position([H|L], Pred, Pos) ->
    case Pred(H) of
        true ->
            Pos;
        false ->
            position(L, Pred, Pos + 1)
    end.

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {write_concurrency, true}]),
    ok.
