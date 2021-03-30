-module(docwatch_bind_explicit).

-export([find/0, print/1]).

%% -------------------------------------------
%% API
%% -------------------------------------------

find() ->
    ets:foldl(fun find_in_file/2, [], docs).

%% -------------------------------------------
%% Debug API
%% -------------------------------------------

print(Bindings) ->
    io:format("~p~n", Bindings).

%% -------------------------------------------
%% Internal functions
%% -------------------------------------------

find_in_file({Filename, Lines}, BindingsAcc) ->
    lists:foldl(fun(<<"<!--dw:bind:", Rest/binary>>, Acc) ->
                                   [parse_binding(Filename, Rest) | Acc];
                              (_, Acc) ->
                                   Acc
                           end, BindingsAcc, Lines).

parse_binding(Filename, <<"doc ", TargetWithTrailer/binary>>) ->
    {Pos, _} = binary:match(TargetWithTrailer, <<"-->">>),
    Target = string:slice(TargetWithTrailer, 0, Pos),
    #{ doc => Filename,
       type => doc,
       target => Target }.

