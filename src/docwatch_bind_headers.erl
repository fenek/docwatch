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
    find_in_file(Filename, Lines, BindingsAcc).

find_in_file(Filename, [<<"#", Rest/binary>> | RLines], Acc) ->
    find_in_file(Filename, RLines, [parse_binding(Filename, Rest, RLines) | Acc]);
find_in_file(Filename, [_ | RLines], Acc) ->
    find_in_file(Filename, RLines, Acc);
find_in_file(_Filename, [], Acc) ->
    Acc.

parse_binding(Filename, Lines, RLines) ->
    io:format("~p~n", [Lines]),
    % io:format("~p~n", [Lines]),
    % io:format("~p~n", [RLines]),
    #{ doc => Filename, type => md, target => nil};

parse_binding(Filename, <<"doc ", TargetWithTrailer/binary>>, _RLines) ->
    #{ doc => Filename,
       type => doc,
       target => sanitize_target(TargetWithTrailer) };
parse_binding(Filename, <<"h ", TargetWithTrailer/binary>>, RLines) ->
    #{ doc => Filename,
       type => h,
       h_name => find_h_name(RLines),
       target => sanitize_target(TargetWithTrailer) }.

sanitize_target(TargetWithTrailer) ->
    {Pos, _} = binary:match(TargetWithTrailer, <<"-->">>),
    Target = string:slice(TargetWithTrailer, 0, Pos),
    unicode:characters_to_list(Target).

find_h_name([<<$#, _/binary>> = HName | _]) -> HName;
find_h_name([_ | RLines]) -> find_h_name(RLines).

analyze_binding(#{ doc := Doc, type := doc, target := Src }, ChSrc, ChDocs) ->
    print_if_only_src_changed(Src, ChSrc, Doc, ChDocs,
                              "You've changed \"~s\", perhaps \"~s\" needs an update?~n", [Src, Doc]);
analyze_binding(#{ doc := Doc, type := h, h_name := HName, target := Src }, ChSrc, ChDocs) ->
    print_if_only_src_changed(Src, ChSrc, Doc, ChDocs,
                              "You've changed \"~s\", perhaps section \"~s\" in \"~s\" needs an update?~n",
                              [Src, HName, Doc]).

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

find_file(File, Paths) ->
    lists:any(fun(Path) -> filename:basename(Path) == File end, Paths).

