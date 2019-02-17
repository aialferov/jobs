-module(jobs_app).
-behaviour(application).

-export([
    start/2,
    stop/1, prep_stop/1
]).

-define(Log, jobs_log).

-define(Listener, jobs_http).

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),
    Port = proplists:get_value(port, Env),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/jobs", jobs_handler, #{}},
            {'_', jobs_handler_bad_request, #{}}
        ]}
    ]),
    {ok, Pid} = cowboy:start_clear(
        ?Listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?Log:info("Listening on port: ~b", [Port]),
    {ok, Pid}.

prep_stop(State) ->
    cowboy:stop_listener(?Listener),
    State.

stop(State) -> State.
