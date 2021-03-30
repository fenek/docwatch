-module(docwatch_bind).

%% TODO: Make a proper behavior

%% API
-export([find/0, analyze/4]).

%% Debug
-export([print/1]).

%% ------------------------------------------------
%% API
%% ------------------------------------------------

find() ->
    lists:foldl(fun(Mod, Acc) -> [{Mod, Mod:find()} | Acc] end, [], bind_mods()).

analyze(ProjectPath, Bindings, ChangedSrc, ChangedDocs) ->
    lists:foreach(fun({Mod, Data}) ->
                          Mod:analyze(ProjectPath, Data, ChangedSrc, ChangedDocs)
                  end, Bindings).

%% ------------------------------------------------
%% Debug API
%% ------------------------------------------------

print(Input) ->
    lists:foreach(fun({M, Bindings}) -> M:print(Bindings) end, Input),
    io:format("~n~n", []).

%% ------------------------------------------------
%% Internal functions
%% ------------------------------------------------

%% TODO: Make dynamic in the future
bind_mods() ->
    [docwatch_bind_explicit].

