-module(docwatch_reader).

-export([load_sources/1, load_docs/1]).

%% -------------------------------------------
%% API
%% -------------------------------------------

load_sources(SrcPath) ->
    ets:new(functions, [named_table, public]),
    case filelib:wildcard("**/*.erl", SrcPath) of
        [] ->
            io:format("No .erl files found in ~p!~n~n", [SrcPath]);
        Files ->
            io:format("~p source files found. Processing...~n", [length(Files)]),
            lists:foreach(fun(File) -> load_source(filename:join(SrcPath, File)) end, Files),
            io:format("~nDone!~n", [])
    end.

load_docs(DocPath) ->
    ets:new(docs, [named_table, public]),
    case filelib:wildcard("**/*.md", DocPath) of
        [] ->
            io:format("No .md files found in ~p!~n~n", [DocPath]);
        Files ->
            io:format("~p documentation files found. Processing...~n", [length(Files)]),
            lists:foreach(fun(File) -> load_doc(filename:join(DocPath, File)) end, Files),
            io:format("~nDone!~n", [])
    end.

%% -------------------------------------------
%% Internal functions
%% -------------------------------------------

load_source(File) ->
    {ok, AST} = epp_dodger:quick_parse_file(File, [{no_fail, false}]),
    Functions = lists:foldl(fun({function, LN, F, A, _}, Acc) -> [{LN, F, A} | Acc];
                               (_, Acc) -> Acc
                            end, [], AST),
    Filename = filename:basename(File),
    ets:insert(functions, {Filename, Functions}),
    io:format("*").

load_doc(File) ->
    {ok, Content} = file:read_file(File),
    Lines = binary:split(Content, <<"\n">>, [global]),
    Filename = filename:basename(File),
    ets:insert(docs, {Filename, Lines}),
    io:format("*").
