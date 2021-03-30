-module(docwatch_bind_headers).

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
    [ModuleName, _] = string:split(Filename, "."),
    case ets:lookup(functions, ModuleName ++ ".erl") of
        [] -> find_in_file(Filename, Lines, BindingsAcc);
        _ -> find_in_file(Filename, Lines, parse_binding(Filename, ModuleName) ++ BindingsAcc)
    end.
    

find_in_file(Filename, [<<"# ", Rest/binary>> | RLines], Acc) ->
    find_in_file(Filename, RLines, parse_binding(Filename, Rest) ++ Acc);
find_in_file(Filename, [_ | RLines], Acc) ->
    find_in_file(Filename, RLines, Acc);
find_in_file(_Filename, [], Acc) ->
    Acc.

parse_binding(Filename, HeaderTitle) when is_binary(HeaderTitle) ->
    parse_binding(Filename, binary_to_list(HeaderTitle));

parse_binding(Filename, HeaderTitle) ->
    case ets:lookup(functions, HeaderTitle ++ ".erl") of
        [] -> [];
        _ -> [#{ doc => Filename, type => doc, target => HeaderTitle ++ ".erl"}]
end.

analyze_binding(#{ doc := Doc, type := doc, target := Src }, ChSrc, ChDocs) ->
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

find_file(File, #{} = PathMap) ->
    find_file(File, maps:keys(PathMap));
find_file(File, Paths) ->
    lists:any(fun(Path) -> filename:basename(Path) == File end, Paths).

