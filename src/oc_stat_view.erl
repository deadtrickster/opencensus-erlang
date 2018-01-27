-module(oc_stat_view).

-export([register/6,
         deregister/1,
         subscribe/1,
         subscribe/6,
         unsubscribe/1,
         registered/1]).

-export([measure_views/1,
         subscribed/0,
         add_sample/5,
         export/1]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

-define(NAME_POS, 2).
-define(SUBSCRIBED_POS, 3).

register(Name, Description, Tags, Measure, Aggregation, Window) ->
    register(Name, Description, Tags, Measure, Aggregation, Window, false).

deregister(Name) ->
    ets:delete(?MODULE, Name).

subscribe(Name) ->
    ets:update_element(?MODULE, Name, {?SUBSCRIBED_POS, true}).

subscribe(Name, Description, Tags, Measure, Aggregation, Window) ->
    register(Name, Description, Tags, Measure, Aggregation, Window, true).

register(Name, Description, Tags, Measure, Aggregation, Window, Subscribed) ->
    NAggregation = normalize_aggregation(Aggregation),
    case ets:insert_new(?MODULE, {Measure, Name, Subscribed, Description, Tags, NAggregation, Window}) of
        true ->
            ok;
        _ ->
            case ets:lookup_element(?MODULE, Name, ?NAME_POS) of
                [{Measure, Name, _, Description, Tags, NAggregation, Window}] ->
                    ok;
                _ -> erlang:error(already_exists)
            end
    end.

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

subscribed() ->
    ets:match_object(oc_stat_view, {'_', '_', true, '_', '_', '_', '_'}).

add_sample(Name, _DesiredTags, Window, Aggregation, Value) ->
    Tags = #{}, %% TODO: use tags from ?CONTEXT
    Window:add_sample(Name, Tags, Aggregation, Value).

export({_Measure, Name, _, Description, Tags, Aggregation, Window}) ->
    #{name => Name,
      description => Description,
      tags => Tags,
      rows => Window:export(Name, Aggregation)}.

normalize_aggregation({Module, Options}) ->
    {Module, Options};
normalize_aggregation(Module) when is_atom(Module) ->
    {Module, []}.

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [bag, named_table, public, {read_concurrency, true}]),
    ok.
