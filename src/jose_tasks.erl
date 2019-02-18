-module(jose_tasks).

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
    Names = [Name || #{<<"name">> := Name} <- Tasks],

    TaskMap = lists:foldl(fun(Task = #{<<"name">> := Name}, Map) ->
        maps:put(Name, Task, Map)
    end, #{}, Tasks),

    State = {[], TaskMap},
    {Flattened, _TaskMap} = lists:foldl(fun flatten_task/2, State, Names),

    lists:reverse(Flattened).

flatten_task(Name, {Flattened0, TaskMap0}) ->
    case maps:take(Name, TaskMap0) of
        {Task, TaskMap1} ->
            Deps = maps:get(<<"requires">>, Task, []),
            {Flattened, TaskMap} =
                lists:foldl(fun flatten_task/2, {Flattened0, TaskMap1}, Deps),
            {[maps:remove(<<"requires">>, Task)|Flattened], TaskMap};
        error ->
            {Flattened0, TaskMap0}
    end.
