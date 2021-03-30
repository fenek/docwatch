-module(docwatch_git).

-export([is_git_installed/0, get_changed_files/1]).

get_changed_files(Path) ->
  case is_git_installed() of
    true ->
      os:cmd("cd " ++ Path ++ " && git diff --name-only HEAD");
    false ->
      {error, "Git not found"}
  end.

is_git_installed() ->
  case os:find_executable("git") of
    false ->
      false;
    _ ->
      true
  end.
