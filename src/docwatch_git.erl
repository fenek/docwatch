-module(docwatch_git).

-export([get_changed_files/1, get_changed_files/2]).

get_changed_files(Path) ->
  case is_git_installed() of
    true ->
      Main = get_default_branch(Path),
      get_changed_files(Path, Main);
    false ->
      {error, "Git not found."}
  end.

get_changed_files(Path, Main) ->
  case is_git_installed() of
    true ->
      Result = os:cmd("cd " ++ Path ++ " && git diff --name-only " ++ Main),
      Trimmmed = string:trim(Result, both),
      FileNames = string:split(Trimmmed, "\n", all),
      FilesThatMatter = lists:filter(fun filter_files_that_matter/1, FileNames),
      {ok, FilesThatMatter};
    false ->
      {error, "Git not found."}
  end.

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
  case re:run(FileName, "\.(erl|hrl|md)") of
    nomatch ->
      false;
    {match, _} ->
      true
  end.
