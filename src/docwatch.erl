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

    init_ets(),
    read_sources(SrcDir),
    read_docs(DocDir),

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

init_ets() ->
    ets:new(sources, [named_table, public]).

read_sources(SrcPath) ->
    case filelib:wildcard("**/*.erl", SrcPath) of
        [] ->
            io:format("No .erl files found in ~p!~n~n", [SrcPath]);
        Files ->
            io:format("~p source files found. Processing...~n", [length(Files)]),
            lists:foreach(fun(File) -> read_source(filename:join(SrcPath, File)) end, Files),
            io:format("~nDone!~n", [])
    end.

read_source(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File, [{no_fail, false}]),
    Functions = lists:foldl(fun({function, LN, F, A, _}, Acc) -> [{LN, F, A} | Acc];
                               (_, Acc) -> Acc
                            end, [], AST),
    Filename = filename:basename(File),
    ets:insert(sources, {Filename, Functions}),
    io:format("*").

read_docs(DocPath) ->
    case filelib:wildcard("**/*.md", DocPath) of
        [] ->
            io:format("No .md files found in ~p!~n~n", [DocPath]);
        Files ->
            io:format("~p documentation files found. Processing...~n", [length(Files)]),
            lists:foreach(fun(File) -> read_doc(filename:join(DocPath, File)) end, Files),
            io:format("~nDone!~n", [])
    end.

read_doc(_) ->
    ok.

