-module(docwatch).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([ProjectPath]) ->
    validate_dir(ProjectPath),
    SrcDir = validate_dir(ProjectPath, "src"),
    DocDir = validate_dir(ProjectPath, "doc"),

    {ok, ChangedSrc} = docwatch_git:get_changed_sources(ProjectPath),
    {ok, ChangedDocs0} = docwatch_git:get_changed_docs(ProjectPath),
    ChangedDocs = sanitize_changed_docs(ProjectPath, ChangedDocs0),

    io:format("Changed sources: ~p~n", [ChangedSrc]),
    io:format("Changed docs: ~p~n", [ChangedDocs]),

    docwatch_reader:load_sources(SrcDir),
    docwatch_reader:load_docs(DocDir),

    Bindings = docwatch_bind:find(),
    %docwatch_bind:print(Bindings),

    docwatch_bind:analyze(ProjectPath, Bindings, ChangedSrc, ChangedDocs),

    erlang:halt(0);
main(_) ->
    io:format("Usage: docwatch /path/to/git/erl/project~n~n", []),
    erlang:halt(3).

%%====================================================================
%% Internal functions
%%====================================================================

validate_dir(Path1, Path2) ->
    validate_dir(filename:join(Path1, Path2)).

validate_dir(Path) ->
    case filelib:is_dir(Path) of
        true ->
            Path;
        false ->
            io:format("~p not found! :(~n~n", [Path]),
            erlang:halt(1)
    end.

sanitize_changed_docs(_DocDir, []) ->
    [];
sanitize_changed_docs(DocDir, [ File | RFiles ]) ->
    case docwatch_git:get_file_diff(DocDir, File) of
        {ok, []} -> sanitize_changed_docs(DocDir, RFiles);
        _ -> [ File | sanitize_changed_docs(DocDir, RFiles) ]
    end.

