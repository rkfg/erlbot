-module(test).
-export([do/2, info/0]).

timed_loop(_, 0) ->
    ok;

timed_loop(From, Counter) ->
    gen_event:notify(manager, {say, From, lists:flatten(io_lib:format("ololo #~w from ~w", [Counter, self()]))}),
    timer:sleep(2000),
    timed_loop(From, Counter - 1).

do(From, _Args) ->
    [Node, Domain, _] = From,
    io:format("Node: ~w, Domain: ~w, we're in test PID ~w~n", [Node, Domain, self()]),
    timed_loop(Node ++ "@" ++ Domain, 10),
    "".

info() ->
    {["test", "тест"], {?MODULE, do}}.
