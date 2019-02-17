-module(jobs_log).

-export([
    request/1, request/2,
    response/2, response/3,

    info/1, info/2
]).

-define(Format,
    "[~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ] ~s~n"
).

request(Req) -> request(Req, "").
request(Req, Body) ->
    info("~s --> '~s' '~s' '~s' '~s'", [
        peer(Req),
        maps:get(method, Req),
        maps:get(path, Req),
        maps:get(qs, Req),
        Body
    ]).

response(Req, Code) -> response(Req, Code, "").
response(Req, Code, Body) ->
    info("~s <-- '~b' '~s'", [peer(Req), Code, Body]).

info(Text) -> info(Text, []).
info(Format, Args) ->
    Text = format(Format, Args),
    Now = {_MegaSecs, _Secs, Mcs} = erlang:timestamp(),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    io:format(?Format, [Y, Mo, D, H, Mi, S, Mcs div 1000, Text]).

peer(Req) ->
    case maps:find(peer, Req) of
        {ok, {{A, B, C, D}, Port}} ->
            format("~b.~b.~b.~b:~b", [A, B, C, D, Port]);
        _Other -> "-"
    end.

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).
