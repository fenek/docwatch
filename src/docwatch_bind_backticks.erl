-module(docwatch_bind_backticks).

-export([find/0, analyze/4]).

-export([print/1]).

%% -------------------------------------------
%% API
%% -------------------------------------------

find() ->
    ets:foldl(fun find_in_file/2, [], docs).

analyze(_ProjectPath, Bindings, ChangedSrc, ChangedDocs) ->
    lists:foreach(fun(Binding) ->
                          analyze_binding(Binding, ChangedSrc, ChangedDocs)
                  end, Bindings).

%% -------------------------------------------
%% Debug API
%% -------------------------------------------

print(Bindings) ->
    io:format("~p~n", [Bindings]).

%% -------------------------------------------
%% Internal functions
%% -------------------------------------------

find_in_file({Filename, Lines}, BindingsAcc) ->
    lists:foldl(fun(Line, Acc) ->
                        %% TODO: Broken for "`some\`thing`"
                        Match = re:run(Line, "\\s`([^`]+)`", [global, {capture, [1], list}]),
                        validate_and_add_bindings(Filename, Match, Acc)
                end, BindingsAcc, Lines).

validate_and_add_bindings(_Filename, nomatch, Acc) ->
    Acc;
validate_and_add_bindings(Filename, {match, Matches}, Acc) ->
    validate_and_add_bindings(Filename, Matches, Acc);
validate_and_add_bindings(_Filename, [], Acc) ->
    Acc;
validate_and_add_bindings(Filename, [[Match] | RMatches], Acc) ->
    NAcc =
    case re:run(Match, ":", [global]) of
        nomatch ->
            case ets:lookup(functions, Match ++ ".erl") of
                [] -> Acc;
                _ ->
                    [#{ doc => Filename, type => module, module => Match } | Acc]
            end;
        {match, [_]} ->
            %% TODO
            Acc;
        _ ->
            Acc
    end,
    validate_and_add_bindings(Filename, RMatches, NAcc).

analyze_binding(#{ doc := Doc, type := module, module := Module }, ChSrc, ChDocs) ->
    Src = Module ++ ".erl",
    print_if_only_src_changed(Src, ChSrc, Doc, ChDocs,
                              "You've changed \"~s\", perhaps \"~s\" needs an update?~n", [Src, Doc]).

%% TODO: SO INEFFICIENT!!!
print_if_only_src_changed(Src, ChSrc, Doc, ChDocs, Fmt, Vars) ->
    case find_file(Src, ChSrc) of
        true ->
            case find_file(Doc, ChDocs) of
                false -> io:format(Fmt, Vars);
                true -> ok
            end;
        false ->
            ok
    end.

find_file(File, #{} = FileMap) ->
    find_file(File, maps:keys(FileMap));
find_file(File, Paths) ->
    lists:any(fun(Path) -> filename:basename(Path) == File end, Paths).

