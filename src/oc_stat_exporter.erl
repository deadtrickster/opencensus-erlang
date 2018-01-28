%% @doc
%% Exporter exports the collected records as view data.
%%
%% The `export' callback should return quickly; if an
%% Exporter takes a significant amount of time to
%% process a ViewData, that work should be done on another process.
%% @end
-module(oc_stat_exporter).

-export([batch_register/1,
         register/1,
         register/2,
         deregister/1,
         registered/1]).

-export([export/1]).

-export(['__init_backend__'/0]).

-compile({no_auto_import, [register/2]}).

-include("opencensus.hrl").

-callback export(ViewData, Config) -> ok when
      ViewData :: view_data(),
      Config  :: any().

%% @doc
%% Register many `Exporters' at once.
%% @end
-spec batch_register(Exporters) -> ok when
      Exporters :: [exporter()].
batch_register(Exporters) when is_list(Exporters) ->
    [register(Exporter, Config) || {Exporter, Config} <- Exporters],
    ok.

%% @doc
%% @equiv register(Exporter, [])
%% @end
-spec register(Exporter) -> ok when
      Exporter :: exporter().
register(Exporter) ->
    register(Exporter, []),
    ok.

%% @doc
%% Registers an `Exporter' with `Config'.
%% Collected data will be reported via all the
%% registered exporters. Once you no longer
%% want data to be exported, invoke {@link deregister/1}
%% with the previously registered exporter.
%% @end
-spec register(Exporter, Config) -> ok when
      Exporter :: exporter(),
      Config  :: any().
register(Exporter, Config) ->
    ets:insert(?MODULE, {Exporter, Config}),
    ok.

%% @doc
%% Deregisters an `Exporter'.
%% @end
-spec deregister(Exporter) -> ok when
      Exporter :: exporter().
deregister(Exporter) ->
    ets:delete(?MODULE, Exporter),
    ok.

%% @doc
%% Checks whether `Exporter' is registered.
%% @end
-spec registered(Exporter) -> boolean() when
      Exporter :: exporter().
registered(Exporter) ->
    ets:lookup(?MODULE, Exporter) =/= [].

'__init_backend__'() ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    ok.

%% @doc
%% @private
%% Called by opencensus
%% @end
export(Measurements) ->
    Exporters = ets:tab2list(?MODULE),
    [try
         Exporter:export(Measurements, Config)
     catch
         Class:Exception ->
             error_logger:info_msg("stat exporter ~p threw ~p:~p, stacktrace=~p",
                                   [Exporter, Class, Exception, erlang:get_stacktrace()])
     end
     || {Exporter, Config} <- Exporters],
    ok.
