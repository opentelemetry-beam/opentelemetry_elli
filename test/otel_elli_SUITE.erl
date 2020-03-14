-module(otel_elli_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/tracer.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").
-include_lib("elli/include/elli.hrl").

all() ->
    [{group, w3c}, {group, b3}].

groups() ->
    [{w3c, [shuffle], [successful_request, error_response]},
     {b3, [shuffle], [successful_request, error_response]}].


init_per_suite(Config) ->
    ok = application:load(opentelemetry_elli),
    ok = application:load(opentelemetry),

    application:set_env(opentelemetry_elli, server_name, <<"my-test-elli-server">>),

    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_group(Propagator, Config) ->
    application:set_env(opentelemetry, processors, [{ot_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    {CorrelationsHttpExtractor, CorrelationsHttpInjector} = ot_correlations:get_http_propagators(),
    {TraceHttpExtractor, TraceHttpInjector} = case Propagator of
                                                  w3c -> ot_tracer_default:w3c_propagators();
                                                  b3 -> ot_tracer_default:b3_propagators()
                                              end,
    opentelemetry:set_http_extractor([CorrelationsHttpExtractor,
                                      TraceHttpExtractor]),
    opentelemetry:set_http_injector([CorrelationsHttpInjector,
                                     TraceHttpInjector]),

    [{propagator, Propagator} | Config].

end_per_group(_, _Config)->
    application:stop(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    elli:start_link([{name, {local, elli_test_server}},
                     {port, 3000},
                     {callback, elli_middleware},
                     {callback_args, [{mods, [{otel_elli_middleware, []},
                                              {?MODULE, []}]}]}]),

    {ok, _} = application:ensure_all_started(opentelemetry),
    ot_batch_processor:set_exporter(ot_exporter_pid, self()),
    Config.

end_per_testcase(_, _Config) ->
    elli:stop(elli_test_server),
    _ = application:stop(opentelemetry),
    ok.

successful_request(_Config) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request("http://localhost:3000/hello/otel"),
    ?assertEqual("Hello otel", Body),

    receive
        {span, #span{name=Name,
                     attributes=Attributes,
                     events=_TimeEvents}} ->
            ?assertEqual(<<"/hello/{who}">>, Name),
            ?assertMatch(#{<<"http.server_name">> := <<"my-test-elli-server">>,
                           <<"http.target">> := <<"/hello/otel">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           %% scheme is removed until fixed in elli
                           %% <<"http.scheme">> := <<"http">>,
                           <<"http.status">> := 200,
                           <<"http.user_agent">> := <<>>,
                           <<"http.method">> := <<"GET">>,
                           <<"net.host.port">> := 3000}, maps:from_list(Attributes))
    after
        5000 ->
            error(timeout)
    end,
    ok.

error_response(_Config) ->
    {ok, {{_, 500, _}, _Headers, _Body}} = httpc:request("http://localhost:3000/error"),

    receive
        {span, #span{name=Name,
                     attributes=Attributes}} ->
            ?assertEqual(<<"/error">>, Name),
            ?assertMatch(#{<<"http.server_name">> := <<"my-test-elli-server">>,
                           <<"http.target">> := <<"/error">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           <<"http.status">> := 500,
                           <<"http.user_agent">> := <<>>,
                           <<"error.message">> := <<"all_hell">>,
                           <<"http.method">> := <<"GET">>}, maps:from_list(Attributes))
    after
        5000 ->
            error(timeout)
    end,
    ok.
%%

handle(Req, Args) ->
    handle(Req#req.path, Req, Args).

handle([<<"hello">>, Who], Req, _Args) ->
    otel_elli:start_span(<<"/hello/{who}">>, Req),
    {ok, [], <<"Hello ", Who/binary>>};
handle([<<"error">>], Req, _Args) ->
    otel_elli:start_span(<<"/error">>, Req),
    throw(all_hell).

handle_event(_Event, _Data, _Args) ->
    ok.
