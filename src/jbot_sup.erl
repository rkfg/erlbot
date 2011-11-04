-module(jbot_sup).

-export([init/1]).
-behavior(supervisor).

init(_) ->
    {ok, {{rest_for_one, 10, 10}, []}}.
