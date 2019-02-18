-module('jose_tasks_test').

-include_lib("eunit/include/eunit.hrl").

-define(M, 'jose_tasks').

flatten_empty_test() ->
    ?assertEqual(
        ?M:flatten(#{<<"tasks">> => []}),
        {ok, []}
    ).

flatten_no_deps_test() ->
    ?assertEqual(
        {ok, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]},
        ?M:flatten(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]})
    ).

flatten_with_deps_test() ->
    ?assertEqual(
        {ok, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>}
        ]},
        ?M:flatten(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>,
              <<"requires">> => [
                <<"task-3">>
              ]},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
              <<"requires">> => [
                <<"task-1">>
              ]},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>,
              <<"requires">> => [
                <<"task-2">>,
                <<"task-3">>
              ]}
        ]})
    ).

flatten_with_more_deps_test() ->
    ?assertEqual(
        {ok, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]},
        ?M:flatten(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>,
              <<"requires">> => [
                <<"task-3">>
              ]},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
              <<"requires">> => [
                <<"task-4">>
              ]},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>,
              <<"requires">> => [
                <<"task-1">>
              ]}
        ]})
    ).

flatten_with_loop_test() ->
    ?assertEqual(
        {ok, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]},
        ?M:flatten(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>,
              <<"requires">> => [
                <<"task-3">>
              ]},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
              <<"requires">> => [
                <<"task-4">>
              ]},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>,
              <<"requires">> => [
                <<"task-2">>
              ]}
        ]})
    ).

flatten_with_self_loop_test() ->
    ?assertEqual(
        {ok, [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>}
        ]},
        ?M:flatten(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>,
              <<"requires">> => [
                <<"task-1">>
              ]}
        ]})
    ).
flatten_invalid_format_test() ->
    ?assertEqual(
        {error, invalid_format},
        ?M:flatten(#{<<"tsks">> => []})
    ).

scriptize_empty_test() ->
    ?assertEqual(
        {ok, <<"#!/bin/bash\n\n">>},
        ?M:scriptize(#{<<"tasks">> => []})
    ).

scriptize_no_deps_test() ->
    ?assertEqual(
        {ok, <<
            "#!/bin/bash\n\n"
            "touch /tmp/file1\n"
            "cat /tmp/file1\n"
        >>},
        ?M:scriptize(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>}
        ]})
    ).

scriptize_with_deps_test() ->
    ?assertEqual(
        {ok, <<
            "#!/bin/bash\n\n"
            "touch /tmp/file1\n"
            "echo 'Hello World!' > /tmp/file1\n"
            "cat /tmp/file1\n"
            "rm /tmp/file1\n"
        >>},
        ?M:scriptize(#{<<"tasks">> => [
            #{<<"name">> => <<"task-1">>,
              <<"command">> => <<"touch /tmp/file1">>},
            #{<<"name">> => <<"task-2">>,
              <<"command">> => <<"cat /tmp/file1">>,
              <<"requires">> => [
                <<"task-3">>
              ]},
            #{<<"name">> => <<"task-3">>,
              <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
              <<"requires">> => [
                <<"task-1">>
              ]},
            #{<<"name">> => <<"task-4">>,
              <<"command">> => <<"rm /tmp/file1">>,
              <<"requires">> => [
                <<"task-2">>,
                <<"task-3">>
              ]}
        ]})

    ).
