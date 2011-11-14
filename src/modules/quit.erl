-module(quit).
-export([do/2, info/0]).

do(_From, _Args) ->
    gen_event:notify(manager, stop),
    "выхожу...".

info() ->
    {"quit", {?MODULE, do}, 0}.
