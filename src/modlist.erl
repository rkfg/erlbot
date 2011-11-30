-module(modlist).
-export([modules/0]).

modules() ->
    [
     quit,
     reload,
     djcal,
     join,
     leave,
     help,
     title
    ].
