elli_otter
=====

[OpenTracing](http://opentracing.io), via [otter](https://github.com/Bluehouse-Technology/otter), middleware for [Elli](https://github.com/elli-lib/elli).

### Install

```erlang
{deps, [elli_otter]}.
```

Use the `elli_middleware` module as the callback with the `elli_otter_middleware` in the mods list, followed by other middlewares and your callback.

```erlang
ElliOtterConfig = elli_otter:config(#{prefix => <<>>,
                                      traced_request_attributes => [path, method, headers],
                                      log_exceptions => true}),
Config = [{mods, [{elli_otter_middleware, ElliOtterConfig}, ...]}],
ElliOpts = [{callback, elli_middleware}, {callback_args, Config}, ...],
```

### Configuration

See the [otter](https://github.com/Bluehouse-Technology/otter) documentation for how to configure filtering and sending to [Zipkin](http://zipkin.io/) or [Jaeger](https://uber.github.io/jaeger/).

`elli_otter` provides the function `config` to return the proper list used for configuration in the middleware. It takes a map with options:

* `prefix`: String the headers in requests will have for OpenTracing specific headers. Default: `<<>>`.
* `traced_request_attributes`: List of a attributes to tag. Default: `[path, method, headers]`.
* `log_exceptions`: If `true` the and an exception happens that Elli handles, like `request_parse_error` if it fails to parse the request, a log of the stacktrace will be included in the span. Default: `true`.
* `debug`: If `true` the span is started for every request. Default: `true`.
* `sampling_percent`:  Percent of incoming requests to create spans for. Default: `10`. Note: When using defaults this one doesnt matter since `debug` is `true`.
