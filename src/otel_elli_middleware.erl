%%%------------------------------------------------------------------------
%% Copyright 2020, Tristan Sloughter
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
%% @doc Elli middleware for tracing requests and recording stats.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_elli_middleware).

-include_lib("elli/include/elli.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([preprocess/2,
         handle/2,
         handle_event/3]).

preprocess(Req=#req{headers=Headers}, _) ->
    %% extract trace context from headers to be used as the parent
    %% of a span if started by the user's elli handler
    Headers = elli_request:headers(Req),
    otel_propagator:text_map_extract(Headers),

    %% TODO: consider starting span here and relying on `update_name'

    Req.

handle(_Req, _Config) ->
    ignore.

handle_event(elli_startup, _Args, _Config) ->
    _ExcludeUrls = case os:getenv("OTEL_ERLANG_ELLI_EXCLUDED_URLS") of
                      false ->
                          [];
                      E ->
                          E
                  end,

    {ok, Vsn} = application:get_key(opentelemetry_elli, vsn),
    _ = opentelemetry:register_tracer(opentelemetry_elli, Vsn),

    %% TODO: create instruments here
    ok;
handle_event(request_complete, Args, Config) ->
    handle_full_response(request_complete, Args, Config);
handle_event(chunk_complete, Args, Config) ->
    handle_full_response(chunk_complete, Args, Config);

handle_event(request_timeout, _, _Config) ->
    finish_exception(request_timeout, request_timeout);
handle_event(request_parse_error, [Reason], _Config) ->
    finish_exception(request_parse_error, Reason);
handle_event(client_closed, [RequestPart], _Config) ->
    finish_exception(client_closed, {request_part, RequestPart});
handle_event(client_timeout, [RequestPart], _Config) ->
    finish_exception(client_timeout, {request_part, RequestPart});
handle_event(bad_request, [Reason], _Config) ->
    finish_exception(bad_request, Reason);
handle_event(request_error, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(request_throw, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(request_exit, [_Req, Exception, Stacktrace], _Config) ->
    finish_exception(Exception, Stacktrace);
handle_event(invalid_return, [_Req, _Unexpected], _Config) ->
    %% TODO: should we include the `Unexpected' response in the attributes?
    %% it could have sensitive data so should be configurable
    finish_exception(<<"invalid_return">>, []),
    ok;
handle_event(_Event, _Args, _Config) ->
    ok.

%%

handle_full_response(_Type, [_Req, Code, _Hs, _B, {_Timings, _Sizes}], _Config) ->
    %% case proplists:get_value(req_body, Sizes) of
    %%     undefined ->
    %%         ok;
    %%     _UncompressedReqSize ->
    %%         %% TODO: record the uncompressed received body size
    %%         ok
    %% end,

    %% _ServerLatency = proplists:get_value(request_end, Timings) -
    %%     proplists:get_value(headers_start, Timings),

    %% _UncompressedRespSize = size(Sizes, response_body),

    Status = opentelemetry:status(http_to_otel_status(Code), <<>>),
    ?set_status(Status),
    ?set_attribute(<<"http.status">>, Code),

    %% end the span that the user might have started
    %% if there is no started span this is a noop
    ?end_span(),

    ok.

%% size(Sizes, response_body) ->
%%     case proplists:get_value(chunks, Sizes) of
%%         undefined ->
%%             case proplists:get_value(file, Sizes) of
%%                 undefined ->
%%                     proplists:get_value(resp_body, Sizes);
%%                 FileSize ->
%%                     FileSize
%%             end;
%%         ChunksSize ->
%%             ChunksSize
%%     end.

finish_exception(Exception, Stacktrace) ->
    ?set_attributes([{<<"stacktrace">>, term_to_string(Stacktrace)},
                     {<<"error.message">>, term_to_string(Exception)}]).

term_to_string(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

http_to_otel_status(Code) when Code >= 100 , Code =< 399 ->
    ?OTEL_STATUS_UNSET;
http_to_otel_status(_) ->
    ?OTEL_STATUS_ERROR.
