-module('jobs').

-export([
    main/1
]).

-include("jobs.hrl").

-define(Usage,
    "Usage: jobs <Command>~n"
    "~n"
    "Commands~n"
    "       run         Run~n"
    "       help        Print this message~n"
    "       version     Print version~n"
    "~n"
).

-define(ConsoleUsage,
    "a|api          Print API reference~n"
    "e|example      Print API usage example~n"
    "h|help         Print this message~n"
    "q|quit|exit    Shutdown the service and exit the console~n"
).

-define(Greeting,
    "Welcome to the Jobs service console!~n"
    "~n"
    "Console usage:~n"
    ?ConsoleUsage
).
-define(Farewell, "Bye.~n").

-define(Prompt, "jobs> ").
-define(PromptExit,
    "This will stop the service "
    "and make API unavailable, continue? [y/n]: "
).

-define(Version, "Version ~s (git-~s)~n").

main(Args) ->
    application:start(?MODULE),
    case Args of
        ["run"] -> run();
        ["help"] -> show_usage();
        ["version"] -> show_version();
        Args -> show_usage()
    end.

run() ->
    application:ensure_all_started(?MODULE),
    cpf_node:start(?MODULE),
    console(),
    cpf_node:stop().

console() ->
    io:format(?Greeting),
    console_loop(),
    io:format(?Farewell).

console_loop() ->
    case read_input(?Prompt) of
        eof -> eof;

        Command when Command == "a";
                     Command == "api" -> io:format(?JobsApiUsage),
                                         console_loop();

        Command when Command == "e";
                     Command == "example" -> show_example(),
                                             console_loop();

        Command when Command == "q";
                     Command == "quit";
                     Command == "exit" -> console_exit();

        Command when Command == "h";
                     Command == "help" -> io:format(?ConsoleUsage),
                                          console_loop();

        _Command -> console_loop()
    end.

console_exit() ->
    case read_input(?PromptExit) of
        "y" -> ok;
        "n" -> console_loop();
        _Other -> console_exit()
    end.
    
read_input(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        Data -> string:trim(Data, both, "\n")
    end.

show_example() ->
    io:format(?JobsExample).

show_usage() ->
    io:format(?Usage).

show_version() ->
    {ok, Vsn} = application:get_key(?MODULE, vsn),
    {ok, GitSha} = application:get_env(?MODULE, git_sha),
    io:format(?Version, [Vsn, GitSha]).
