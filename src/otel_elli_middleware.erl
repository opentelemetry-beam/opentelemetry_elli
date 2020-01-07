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

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([preprocess/2,
         handle/2,
         handle_event/3]).

preprocess(Req=#req{raw_path=_RawPath, method=_Method}, _) ->
    Req.

handle(_Req, _Config) ->
    ignore.

handle_event(elli_startup, _Args, _Config) ->
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
handle_event(_Event, _Args, _Config) ->
    ok.

%%

handle_full_response(_Type, [_Req, Code, _Hs, _B, {_Timings, _Sizes}], _Config) ->
    ocp:update_tags(#{http_server_status => integer_to_list(Code)}),

    case proplists:get_value(req_body, Sizes) of
        undefined ->
            ok;
        _UncompressedReqSize ->
            ok
    end,
    _ServerLatency = proplists:get_value(request_end, Timings) - proplists:get_value(headers_start, Timings),

    _UncompressedRespSize = size(Sizes, response_body),

    ok.

size(Sizes, response_body) ->
    case proplists:get_value(chunks, Sizes) of
        undefined ->
            case proplists:get_value(file, Sizes) of
                undefined ->
                    proplists:get_value(resp_body, Sizes);
                FileSize -> FileSize
            end;
        ChunksSize -> ChunksSize
    end.

finish_exception(_Exception, _Stacktrace) ->
    ok.

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

term_to_string(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).
