-module(reload).
-export([do/2, info/0]).

do(_From, _Args) ->
    gen_event:sync_notify(manager, load_commands),
    gen_event:sync_notify(manager, reload),
    "reloading...".

info() ->
    {"reload", {?MODULE, do}}.
