-module(jobs_handler_bad_request).

-export([
    init/2,
    reply/1, reply/2
]).

-include("jobs.hrl").
-include("jobs_handler.hrl").

-define(Log, jobs_log).

init(Req, State) -> {ok, reply(Req), State}.

reply(Req0) ->
    {ok, Req} = log_request(Req0),
    log_response(Req, "<Usage>"),

    Response = lists:flatten(io_lib:format(?JobsApiUsage, [])),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeText, Response, Req).

reply(Reason, Req0) ->
    {ok, Req} = log_request(Req0),
    log_response(Req, Reason),

    Response = jsx:encode(#{reason => Reason}),
    cowboy_req:reply(?CodeBadRequest, ?ContentTypeJson, Response, Req).

log_request(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    ?Log:request(Req, Body),
    {ok, Req}.

log_response(Req, Response) ->
    ?Log:response(Req, ?CodeBadRequest, Response).
