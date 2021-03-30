-module(docwatch_bind_explicit).

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
    find_in_file(Filename, Lines, BindingsAcc).

find_in_file(Filename, [<<"<!--dw:bind:", Rest/binary>> | RLines], Acc) ->
    find_in_file(Filename, RLines, [parse_binding(Filename, Rest, RLines) | Acc]);
find_in_file(Filename, [_ | RLines], Acc) ->
    find_in_file(Filename, RLines, Acc);
find_in_file(_Filename, [], Acc) ->
    Acc.

parse_binding(Filename, <<"doc ", TargetWithTrailer/binary>>, _RLines) ->
    #{ doc => Filename,
       type => doc,
       target => parse_mfa(sanitize_target(TargetWithTrailer)) };
parse_binding(Filename, <<"h ", TargetWithTrailer/binary>>, RLines) ->
    #{ doc => Filename,
       type => h,
       h_name => find_h_name(RLines),
       target => parse_mfa(sanitize_target(TargetWithTrailer)) }.

sanitize_target(TargetWithTrailer) ->
    {Pos, _} = binary:match(TargetWithTrailer, <<"-->">>),
    Target = string:slice(TargetWithTrailer, 0, Pos),
    unicode:characters_to_list(Target).

parse_mfa(Target) ->
    case string:tokens(Target, ":") of
        [_] -> Target;
        [M, FA] ->
            case string:tokens(FA, "/") of
                [_] -> {M ++ ".erl", list_to_atom(FA)};
                [F, A] -> {M ++ ".erl", list_to_atom(F), list_to_integer(A)}
            end
    end.

find_h_name([<<$#, _/binary>> = HName | _]) -> HName;
find_h_name([_ | RLines]) -> find_h_name(RLines).

analyze_binding(#{ doc := Doc, type := doc, target := Target }, ChSrc, ChDocs) ->
    print_if_only_src_changed(Target, ChSrc, Doc, ChDocs,
                              "You've changed \"~s\", perhaps \"~s\" needs an update?~n",
                              [pp_target(Target), Doc]);
analyze_binding(#{ doc := Doc, type := h, h_name := HName, target := Target }, ChSrc, ChDocs) ->
    print_if_only_src_changed(Target, ChSrc, Doc, ChDocs,
                              "You've changed \"~s\", perhaps section \"~s\" in \"~s\" needs an update?~n",
                              [pp_target(Target), HName, Doc]).

pp_target({M, F, A}) ->
    pp_m(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
pp_target({M, F}) ->
    pp_m(M) ++ ":" ++ atom_to_list(F);
pp_target(M) ->
    M.

pp_m(M) ->
    string:trim(M, trailing, ".erl").

%% TODO: SO INEFFICIENT!!!
print_if_only_src_changed(Src, ChSrc, Doc, ChDocs, Fmt, Vars) ->
    case match_mfa(Src, ChSrc) of
        true ->
            case find_file(Doc, ChDocs) of
                false -> io:format(Fmt, Vars);
                true -> ok
            end;
        false ->
            ok
    end.

match_mfa({M, F, A}, ChSrc) ->
    case maps:get(M, ChSrc, undefined) of
        undefined ->
            false;
        FAs ->
            lists:member({F, A}, FAs)
    end;
match_mfa({M, F}, ChSrc) ->
    case maps:get(M, ChSrc, undefined) of
        undefined ->
            false;
        FAs ->
            lists:keyfind(F, 1, FAs) /= false
    end;
match_mfa(M, ChSrc) ->
    maps:is_key(M, ChSrc).

find_file(File, Paths) ->
    lists:any(fun(Path) -> filename:basename(Path) == File end, Paths).

