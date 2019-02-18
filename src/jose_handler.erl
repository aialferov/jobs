-module(jose_handler).
-export([init/2]).

-include("jose_handler.hrl").

-define(Tasks, jose_tasks).
-define(Log, jose_log).

-define(HandlerBadRequest, jose_handler_bad_request).

init(Req0 = #{method := Method, has_body := true}, State) when
    Method == <<"POST">>
->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Query = cowboy_req:parse_qs(Req1),

    ?Log:request(Req1, Body),

    Req = case decode_body(Body) of
        {ok, Json} ->
            case handle_request(Query, Json) of
                {ok, Response} -> reply_data(Response, Req1);
                {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1)
            end;
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1)
    end,
    {ok, Req, State};

init(Req, State) ->
    ?HandlerBadRequest:init(Req, State).

handle_request(Query, Json) ->
    case Query of
        [] -> ?Tasks:scriptize(Json);
        [{<<"tasks">>, <<"scriptize">>}] -> ?Tasks:scriptize(Json);
        [{<<"tasks">>, <<"flatten">>}] -> ?Tasks:flatten(Json);
        Query -> {error, invalid_query}
    end.

reply_data(Data, Req) ->
    IsBin = is_binary(Data),
    {ContentType, Body} = if IsBin -> {?ContentTypeText, Data};
                         not IsBin -> {?ContentTypeJson, jsx:encode(Data)} end,

    ?Log:response(Req, ?CodeOk, Body),
    cowboy_req:reply(?CodeOk, ContentType, Body, Req).

decode_body(Body) ->
    try jsx:decode(Body, [return_maps]) of
        DecodedBody -> {ok, DecodedBody}
    catch
        _:_ -> {error, malformed_json}
    end.
