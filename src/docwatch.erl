-module(docwatch).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([ProjectPath]) ->
    ensure_dir(ProjectPath),
    SrcDir = ensure_dir(ProjectPath, "src"),
    DocDir = ensure_dir(ProjectPath, "doc"),

    docwatch_reader:load_sources(SrcDir),
    docwatch_reader:load_docs(DocDir),

    Bindings = docwatch_bind:find(),

    docwatch_bind:print(Bindings),

    erlang:halt(0);
main(_) ->
    io:format("Usage: docwatch /path/to/git/erl/project~n~n", []),
    erlang:halt(3).

%%====================================================================
%% Internal functions
%%====================================================================

ensure_dir(Path1, Path2) ->
    ensure_dir(filename:join(Path1, Path2)).

ensure_dir(Path) ->
    case filelib:is_dir(Path) of
        true ->
            Path;
        false ->
            io:format("~p not found! :(~n~n", [Path]),
            erlang:halt(1)
    end.

