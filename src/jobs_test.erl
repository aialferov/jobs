-module('jobs_test').

-include_lib("eunit/include/eunit.hrl").

-define(M, 'jobs').

main_test() ->
    ?assertEqual(ok, ?M:main([])).
