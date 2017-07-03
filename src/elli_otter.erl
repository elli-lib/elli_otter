-module(elli_otter).

-export([config/1,
         ids/0,
         get_span/0]).

-include("elli_otter.hrl").

-spec config(maps:map()) -> #elli_otter_config{}.
config(Map) ->
    #elli_otter_config{prefix = maps:get(prefix, Map, <<"">>),
                       traced_request_attributes = maps:get(traced_request_attributes, Map, [path, method, headers]),
                       log_exceptions = maps:get(log_exceptions, Map, true),
                       sampling_percent = max(0, min(100, maps:get(sampling_percent, Map, 10))),
                       debug = maps:get(debug, Map, true)}.

-spec ids() -> {otter:trace_id(), otter:span_id()}.
ids() ->
    otter_span_pdict_api:ids().

-spec get_span() -> otter:span().
get_span() ->
    otter_span_pdict_api:get_span().
