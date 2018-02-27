%%%------------------------------------------------------------------------
%% Copyright 2018, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Exports spans to Zipkin
%% @end
%%%------------------------------------------------------------------------


-module(oc_zipkin_reporter).

-include("opencensus.hrl").

-behaviour(oc_reporter).

-define(DEFAULT_ZIPKIN_ADDRESS, "http://localhost:9411").
-define(DEFAULT_LOCAL_ENDPOINT, #{<<"serviceName">> => node()}).

-export([init/1,
         report/2]).

init(Opts) ->
    Opts.

report(Spans, _Opts) ->
    Address = zipkin_address(),
    ZSpans = [zipkin_span(Span) || Span <- Spans],

    try jsx:encode(ZSpans) of
        JSON ->
            case httpc:request(post,
                               {Address ++ "/api/v2/spans", [],
                                "application/json",
                                JSON
                               }, [], []) of
                {ok, {{_, 202, _}, _, _}} ->
                    ok;
                {ok, {{_, Code, _}, _, Message}} ->
                    report_error("Unable to send spans, Zipkin reported an error: ~p : ~p", [Code, Message]);

                {error, Reason} ->
                    report_error("Unable to send spans, client error: ~p", [Reason])
            end
    catch
        error:Error ->
            report_error("Can't spans encode to json: ~p", [Error])
    end.

zipkin_span(Span) ->

    ParentId = case Span#span.parent_span_id of
                   undefined -> null;
                   Id -> iolist_to_binary(io_lib:format("~16.16.0b", [Id]))
               end,
    #{
       <<"traceId">> => iolist_to_binary(io_lib:format("~32.16.0b", [Span#span.trace_id])),
       <<"name">> => iolist_to_binary(Span#span.name),
       <<"parentId">> => ParentId,
       <<"id">> => iolist_to_binary(io_lib:format("~16.16.0b", [Span#span.span_id])),
       <<"kind">> => <<"SERVER">>, %% TODO: get from attributes?
       <<"timestamp">> => wts:to_absolute(Span#span.start_time),
       <<"duration">> => wts:duration(Span#span.start_time, Span#span.end_time),
       <<"debug">> => false, %% TODO: get from attributes?
       <<"shared">> => false, %% TODO: get from attributes?
       <<"localEndpoint">> => local_endpoint(),
       %% <<"remoteEndpoint">> =>  %% TODO: get from attributes?
       <<"annotations">> => [],
       <<"tags">> => to_tags(Span#span.attributes) %% TODO: merge with oc_tags?
     }.

to_tag(_Name, Value) when is_function(Value) ->
    Value();
to_tag(_Name, Value) ->
    Value.

to_tags(Attributes) ->
    maps:map(fun(Name, Value) ->
                     to_tag(Name, Value)
             end, Attributes).

report_error(Message, Reason) ->
    erlang:error({zipkin_error, Message,  Reason}).

zipkin_address() ->
    proplists:get_value(address, conf(), ?DEFAULT_ZIPKIN_ADDRESS).

local_endpoint() ->
    proplists:get_value(local_endpoint, conf(), ?DEFAULT_LOCAL_ENDPOINT).

conf() ->
    application:get_env(opencensus, zipkin_reporter, []).
