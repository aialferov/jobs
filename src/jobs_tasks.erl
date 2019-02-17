-module(jobs_tasks).

-export([
    flatten/1,
    scriptize/1,
    validate/1
]).

flatten(Json) ->
    case validate(Json) of
        ok -> {ok, flatten_tasks(maps:get(<<"tasks">>, Json))};
        {error, Reason} -> {error, Reason}
    end.

scriptize(Json) ->
    case validate(Json) of
        ok -> {ok, iolist_to_binary([<<"#!/bin/bash\n\n">>,[
            <<Command/binary, $\n>> || #{<<"command">> := Command}
            <- flatten_tasks(maps:get(<<"tasks">>, Json))
        ]])};
        {error, Reason} -> {error, Reason}
    end.

validate(Json) ->
    case maps:find(<<"tasks">>, Json) of
        {ok, _Tasks} -> ok; % TODO task list validation
        error -> {error, invalid_format}
    end.

flatten_tasks(Tasks) ->
    State = {[], task_map(Tasks)},
    {Flattened, _TaskMap} = lists:foldl(fun flatten_task/2, State, Tasks),
    lists:reverse(Flattened).

flatten_task(Task, State) ->
    NewState = flatten_task_add_deps(maps:get(<<"requires">>, Task, []), State),
    flatten_task_add_task(maps:get(<<"name">>, Task), NewState).

flatten_task_add_deps(Deps, {Flattened, TaskMap}) ->
    lists:foldl(fun flatten_task_add_task/2, {Flattened, TaskMap}, Deps).

flatten_task_add_task(Name, {Flattened, TaskMap}) ->
    case maps:take(Name, TaskMap) of
        {Task, NewTaskMap} -> {[Task|Flattened], NewTaskMap};
        error -> {Flattened, TaskMap}
    end.

task_map(Tasks) ->
    lists:foldl(fun(Task, Map) ->
        Name = maps:get(<<"name">>, Task),
        maps:put(Name, maps:remove(<<"requires">>, Task), Map)
    end, #{}, Tasks).
