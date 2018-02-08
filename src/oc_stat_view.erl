-module(oc_stat_view).

-export([register/5,
         deregister/1,
         subscribe/1,
         subscribe/5,
         subscribed/1,
         unsubscribe/1,
         registered/1]).

-export([measure_views/1,
         subscribed/0,
         add_sample/3,
         export/1]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

-define(NAME_POS, 2).
-define(SUBSCRIBED_POS, 3).

register(Name, Description, Tags, Measure, Aggregation) ->
    %% TODO: check Measure exists?
    register(Name, Description, Tags, Measure, Aggregation, false).

deregister(Name) ->
    ets:delete(?MODULE, Name).

subscribe(Name) ->
    ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, true}).

subscribe(Name, Description, Tags, Measure, Aggregation) ->
    %% TODO: check Measure exists?
    register(Name, Description, Tags, Measure, Aggregation, true).

register(Name, Description, Tags, Measure, Aggregation, Subscribed) ->
    NAggregation = normalize_aggregation(Aggregation),
    NTags = normalize_tags(Tags),
    case ets:match_object(?MODULE, {Measure, Name, '_', '_', '_', '_'}) of
        [{Measure, Name, '_', Description, NTags, NAggregation}] ->
            ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, Subscribed});
        [_] ->
            erlang:error({already_exists, Name});
        _ ->
            ets:insert(?MODULE, {Measure, Name, Subscribed, Description, NTags, NAggregation})
    end,
    ok.

unsubscribe(Name) ->
    ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, false}).

%% @doc
%% Checks whether a view `Name' is registered.
%% @end
-spec registered(Name) -> boolean() when
      Name :: view_name().
registered(Name) ->
    ets:lookup_element(?MODULE, Name, ?NAME_POS) =/= [].

measure_views(Measure) ->
    ets:lookup(?MODULE, Measure).

subscribed({_Measure, _Name, Subscribed, _Description, _Tags, _Aggregation}) ->
    Subscribed.

subscribed() ->
    ets:match_object(?MODULE, {'_', '_', true, '_', '_', '_'}).

add_sample({_Measure, Name, _Subscribed, _Description, ViewTags, Aggregation}, ContextTags, Value) ->
    {CTags, Keys} = ViewTags,
    Tags = maps:merge(maps:with(Keys, ContextTags), CTags),
    {AggregationModule, AggregationOptions} = Aggregation,
    AggregationModule:add_sample(Name, Tags, Value, AggregationOptions).

export({_Measure, Name, _, Description, Tags, Aggregation}) ->
    {AggregationModule, AggregationOptions} = Aggregation,
    #{name => Name,
      description => Description,
      aggregation => Aggregation,
      tags => Tags,
      rows => AggregationModule:export(Name, AggregationOptions)}.

normalize_aggregation({Module, Options}) ->
    {Module, Options};
normalize_aggregation(Module) when is_atom(Module) ->
    {Module, []}.

normalize_tags([]) ->
    {#{}, []};
normalize_tags(Tags) ->
    normalize_tags(Tags, {#{}, []}).

normalize_tags([], {Map, List}) ->
    {Map, lists:reverse(List)};
normalize_tags([First|Rest], {Map, List}) when is_map(First) ->
    normalize_tags(Rest, {maps:merge(Map, First), List});
normalize_tags([First|Rest], {Map, List}) when is_atom(First) ->
    normalize_tags(Rest, {Map, [First | List]}).

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [bag, named_table, public, {read_concurrency, true}]),
    ok.
