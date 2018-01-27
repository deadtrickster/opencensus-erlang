%% @doc
%% Measure represents a type of metric to be tracked and recorded.
%% For example, latency, request Mb/s, and response Mb/s are measures
%% to collect from a server.
%%
%% Each measure needs to be find before being used.
%% Measure constructors such as `oc_stat_measure:new'
%% automatically registers the measure by the given name.
%% Each registered measure needs to be unique by name.
%% Measures also have a description and a unit.
%% @end
-module(oc_stat_measure).

-export([batch_new/1,
         new/2,
         new/3,
         delete/1,
         find/1]).

-export(['__init_backend__'/0]).

-include("opencensus.hrl").

-callback export(ViewData, Description) -> ok when
      ViewData :: view_data(),
      Description  :: any().

%% @doc
%% Creates many `Measures' at once.
%% @end
-spec batch_new(Measures) -> ok when
      Measures :: [exporter()].
batch_new(Measures) when is_list(Measures) ->
    [new(Name, Description, Unit) || {Name, Description, Unit} <- Measures],
    ok.

%% @doc
%% @equiv new(Name, Description, "")
%% @end
-spec new(Name, Description) -> ok when
      Name :: measure_name(),
      Description :: measure_description().
new(Name, Description) ->
    new(Name, Description, ""),
    ok.

%% @doc
%% Creates measure with `Name', `Description', and `Unit'.
%% Raises `already_exists' if a measure with the same name
%% but different `Description' and/or `Unit' already exists.
%% @end
-spec new(Name, Description, Unit) -> ok | no_return() when
      Name :: measure_name(),
      Description  :: measure_description(),
      Unit :: measure_unit().
new(Name, Description, Unit) ->
    case ets:insert_new(?MODULE, {Name, Description, Unit}) of
        true -> ok;
        false ->
            case ets:lookup(?MODULE, Name) of
                [{Name, Description, Unit}] ->
                    ok;
                _ -> erlang:error(already_exists)
            end
    end.

%% @doc
%% Deletes an existing measure to allow for creation of a new
%% measure with the same name. It returns an error if the measure cannot be
%% deleted, such as one or multiple registered views refer to it.
%% @end
-spec delete(Name) -> ok when
      Name :: exporter().
delete(Name) ->
    %% TODO: check registered views
    ets:delete(?MODULE, Name).

%% @doc
%% Returns the registered measure associated with `Name'.
%% If no registered measure is not found, `false' is returned.
%% @end
-spec find(Name) -> #measure{} when
      Name :: measure_name().
find(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Description, Unit}] ->
            #measure{name=Name, description=Description, unit=Unit};
        _ ->
            false
    end.

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    ok.
