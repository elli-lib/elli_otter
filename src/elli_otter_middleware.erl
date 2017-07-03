-module(elli_otter_middleware).

-behaviour(elli_handler).

-include("elli_otter.hrl").
-include_lib("elli/include/elli.hrl").

-export([preprocess/2,
         handle/2,
         handle_event/3,
         postprocess/3]).

-define(status(Res), element(1, Res)).

preprocess(Req, Config=#elli_otter_config{prefix=Prefix}) ->
    TraceId = elli_request:get_header(<<Prefix/binary, "traceid">>, Req, undefined),
    ParentId = elli_request:get_header(<<Prefix/binary, "parentid">>, Req, undefined),
    case {TraceId, ParentId} of
        {undefined, undefined} ->
            case do_span(Req, Config) of
                true ->
                    start_span(Req, Config, []);
                false ->
                    ok
            end;
        {TraceId, undefined} ->
            start_span(Req, Config, [TraceId]);
        {undefined, ParentId} ->
            start_span(Req, Config, [ParentId]);
        {TraceId, ParentId} ->
            start_span(Req, Config, [TraceId, ParentId])
    end.

start_span(Req=#req{raw_path=RawPath, method=Method}, #elli_otter_config{traced_request_attributes=Attributes}, Args) ->
    Operation = <<(to_binary(Method))/binary, ":", RawPath/binary>>,
    Tags = tags(Attributes, Req),
    erlang:apply(otter_span_pdict_api, start_with_tags, [Operation, Tags | Args]).

%% Called when existing trace context was not in the headers this function checks if
%% we should create a new one if debug is true or we meet the sampling criteria.
do_span(_Req, #elli_otter_config{debug=Debug,
                                 sampling_percent=Percent}) when Debug =:= true
                                                               ; Percent =:= 100 ->
    true;
do_span(Req, #elli_otter_config{prefix=Prefix,
                                sampling_percent=Percent}) ->
    to_bool(elli_request:get_header(<<Prefix/binary, "debug">>, Req, false))
        orelse rand:uniform(100) - 1 < Percent.

postprocess(_Req, Res, _Config) ->
    S = ?status(Res),
    %% error is a special tag to zipkin, so nice to include if it is an error status
    case S >= 400 of true -> otter_span_pdict_api:tag(<<"error">>, S); _ -> ok end,
    otter_span_pdict_api:tag(<<"http.status">>, S),
    otter_span_pdict_api:finish(),
    Res.

handle(_Req, _Config) ->
    ignore.

handle_event(request_closed, _, Config) ->
    finish_exception(request_closed, request_closed, Config);
handle_event(request_timeout, _, Config) ->
    finish_exception(request_timeout, request_timeout, Config);
handle_event(request_parse_error, [Reason], Config) ->
    finish_exception(request_parse_error, Reason, Config);
handle_event(client_closed, [RequestPart], Config) ->
    finish_exception(client_closed, {request_part, RequestPart}, Config);
handle_event(client_timeout, [RequestPart], Config) ->
    finish_exception(client_timeout, {request_part, RequestPart}, Config);
handle_event(bad_request, [Reason], Config) ->
    finish_exception(bad_request, Reason, Config);
handle_event(request_error, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(request_throw, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(request_exit, [_Req, Exception, Stacktrace], Config) ->
    finish_exception(Exception, Stacktrace, Config);
handle_event(_Event, _Args, _Config) ->
    ok.

%%

finish_exception(Exception, Log, #elli_otter_config{log_exceptions=true}) ->
    otter_span_pdict_api:log(term_to_string(Log)),
    otter_span_pdict_api:tag(<<"error">>, term_to_string(Exception)),
    otter_span_pdict_api:finish();
finish_exception(_Exception, _Log, _) ->
    otter_span_pdict_api:finish().

tags(TracedRequestAttributes, Req) ->
    tags(TracedRequestAttributes, Req, []).

tags([], _, Acc) ->
    [{<<"span.kind">>, <<"server">>} | Acc];
tags([path | Rest], Req=#req{raw_path=Path}, Acc) ->
    tags(Rest, Req, [{<<"http.url">>, Path}, {<<"http.uri">>, Path} | Acc]);
tags([method | Rest], Req=#req{method=Path}, Acc) ->
    tags(Rest, Req, [{<<"http.method">>, Path} | Acc]);
tags([headers | Rest], Req=#req{headers=Headers}, Acc) ->
    tags(Rest, Req, Headers++Acc);
tags([_ | Rest], Req, Acc) ->
    tags(Rest, Req, Acc).

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

to_bool(<<"true">>) -> true;
to_bool(true) -> true;
to_bool(_) -> false.

term_to_string(Term) ->
    io_lib:format("~p", [Term]).
