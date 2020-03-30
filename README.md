# opentelemetry_elli

![Common Test](https://github.com/opentelemetry-beam/opentelemetry_elli/workflows/Common%20Test/badge.svg) [![Gitter](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Elli middleware for OpenCensus instrumentation.

## Setup and Configuration

``` erlang
{deps, [opentelemetry_elli]}.
```

Using the elli_middleware callback place oc_elli_middelware as the first module to be called in the list of handlers:

``` erlang
[{callback, elli_middleware},
 {callback_args, [{mods, [{otel_elli_middleware, []},
                          {<YOUR HANDLER>, []}]}]}]
```



OpenTelemetry's [HTTP Semantic Conventions](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md#http-server) for a server details the attributes added to a Span automatically by using this middleware. One such attribute must be set in your `sys.config` under the `opentelemetry_elli` application like:

``` erlang
{opentelemetry_elli, [{server_name, <<"my-http-server">>}]}.
```

It is strongly recommended to set this environment variable so the attribute can be included:

> http.server_name has shown great value in practice, as bogus HTTP Host headers occur often in the wild. It is strongly recommended to set http.server_name to allow associating requests with some logical server entity.

## Use

Because Elli has no router it is not possible to set the name of the Span in the middleware. See the OpenTelemetry docs [Semantic conventions for HTTP spans](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md#name) for why you don't want to set the Span name to the raw path of the request. Thus, the child Span for a request is not started in the middleware but must be done in the Elli handler's `handle` function. However, ending the Span is handled by the middleware. It won't break anything if you end the Span outside of the middleware, but it will mean the Span won't have attributes like `http.status` that are only available when the response is ready to send.

A function `otel_elli:start_span/2` is provided to handle setting all the HTTP related attributes for the span:

``` erlang
handle(Req, Args) ->
    handle(Req#req.path, Req, Args).

handle([<<"hello">>, Who], Req, _Args) ->
    otel_elli:start_span(<<"/hello/{who}">>, Req),
    {ok, [], <<"Hello ", Who/binary>>}.
```

The middleware takes care of extracting the parent Span from the requests headers, both the [W3C](https://w3c.github.io/trace-context/) and [B3](https://github.com/openzipkin/b3-propagation) formats are supported, and that parent will be automatically used when the Span is started.

Other attributes set by the middleware can be found in the OpenTelemetry docs [Semantic conventions for HTTP spans](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md).

## Testing

A Common Test suite which starts an Elli server and tests the exported Spans created by a test handle is found under `test/`. Run with:

``` erlang
$ rebar3 ct
```
