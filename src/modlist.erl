-module(modlist).
-export([modules/0]).

modules() ->
    [
     quit,
     test,
     reload,
     djcal
    ].
