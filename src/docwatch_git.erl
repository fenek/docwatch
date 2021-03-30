-module(docwatch_git).

-export([get_changed_files/1, get_changed_files/2, get_file_diff/2, get_file_diff/3]).

%% --------------------------------------
%% API
%% --------------------------------------

get_changed_files(Path) ->
  case is_git_installed() of
    true ->
      Main = get_default_branch(Path),
      get_changed_files(Path, Main);
    false ->
      {error, "Git not found"}
  end.

get_changed_files(Path, Main) ->
  case is_git_installed() of
    true ->
      FilesThatMatter = do_get_files(Path, Main),
      LinesFromFiles = do_get_lines(Path, FilesThatMatter),
      {ok, LinesFromFiles};
    false ->
      {error, "Git not found"}
  end.

do_get_files(Path, Main) ->
  Result = os:cmd("cd " ++ Path ++ " && git diff --name-only " ++ Main),
  Trimmmed = string:trim(Result, both),
  FileNames = string:split(Trimmmed, "\n", all),
  lists:filter(fun filter_files_that_matter/1, FileNames).

do_get_lines(Path, FileNames) ->
  lists:foldl(fun(FileName, Acc) -> 
    Functions = get_functions(Path, FileName),
    maps:put(FileName, Functions, Acc)   
  end, #{}, FileNames).

get_functions(Path, FileName) ->
  CmdResult = os:cmd("git diff -U0 " ++ Path ++ "/" ++ FileName),
  Trimmed = string:trim(CmdResult, both),
  Splitted = string:split(Trimmed, "\n", all),
  Filtered = lists:filter(fun (Line) -> 
    case re:run(Line, "^@@ -([0-9]+)(,([0-9]+))? [+]([0-9]+)(,([0-9]+))? @@", [{capture, none}]) of
      nomatch -> false;
      match -> true
    end
  end, Splitted),
  Functions = lists:filtermap(fun (Line) -> 
    case string:split(Line, "@@", all) of
      [_, _, []] -> false;
      [_, _, ParsedFunctionName] ->
        TrimmedFunctionName = string:trim(ParsedFunctionName, both),
        [FunctionName, RestOfString] = string:split(TrimmedFunctionName, "(", all),
        ArityOfFunction = calculate_function_arity(RestOfString),
        JoinedFunctions = string:join([FunctionName, integer_to_list(ArityOfFunction)], "/"),
        {true, JoinedFunctions}
    end
  end, Filtered),
  lists:usort(Functions).

calculate_function_arity(String) ->
  case string:split(String, ")", all) of
    [[], _] -> 0;
    [Args, _] ->
      length(string:split(Args, ",", all))
  end.

get_file_diff(Path, File) ->
    get_file_diff(Path, File, get_default_branch(Path)).

get_file_diff(Path, File, Main) ->
    Result = os:cmd("cd " ++ Path ++ " && git diff " ++ Main ++ " -- " ++ File),
    Lines = string:split(string:trim(Result, both), "\n", all),
    {ok, lists:filter(fun lines_that_matter/1, Lines)}.

%% --------------------------------------
%% Internal functions
%% --------------------------------------

is_git_installed() ->
  case os:find_executable("git") of
    false ->
      false;
    _ ->
      true
  end.

get_default_branch(Path) ->
  NotTrimmed =
    os:cmd("cd "
           ++ Path
           ++ " && git symbolic-ref refs/remotes/origin/HEAD | cut -d '/' "
              "-f4"),
  string:trim(NotTrimmed, both).

filter_files_that_matter(FileName) ->
  case re:run(FileName, "\.(erl|hrl)") of
    nomatch ->
      false;
    {match, _} ->
      true
  end.

% TODO: This one shouldn't be hardcoded
lines_that_matter([_ | "<!--dw:bind" ++ _]) -> false;
lines_that_matter([$- | [T1, T2 | _]]) when [T1, T2] /= "--" -> true;
lines_that_matter([$+ | [T1, T2 | _]]) when [T1, T2] /= "++" -> true;
lines_that_matter(_) -> false.

