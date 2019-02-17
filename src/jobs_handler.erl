-module(jobs_handler).
-export([init/2]).

-include("jobs_handler.hrl").

-define(Tasks, jobs_tasks).
-define(Log, jobs_log).

-define(HandlerBadRequest, jobs_handler_bad_request).

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
                {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1, State)
            end;
        {error, Reason} -> ?HandlerBadRequest:reply(Reason, Req1, State)
    end,
    {ok, Req, State};

init(Req, State) ->
    ?HandlerBadRequest:init(Req, State).

handle_request(Query, Json) ->
    case Query of
        [{<<"tasks">>, <<"flatten">>}] -> ?Tasks:flatten(Json);
        [{<<"tasks">>, <<"scriptize">>}] -> ?Tasks:scriptize(Json);
        Query -> {error, invalid_query}
    end.

reply_data(Data, Req) ->
    IsMap = is_map(Data),
    {ContentType, Body} = if IsMap -> {?ContentTypeJson, jsx:encode(Data)};
                         not IsMap -> {?ContentTypeText, Data} end,

    ?Log:response(Req, ?CodeOk, Body),
    cowboy_req:reply(?CodeOk, ContentType, Body, Req).

decode_body(Body) ->
    try jsx:decode(Body, [return_maps]) of
        DecodedBody -> {ok, DecodedBody}
    catch
        _:_ -> {error, malformed_json}
    end.
