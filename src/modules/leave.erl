-module(leave).
-export([do/2, info/0]).
-define(NICK, "erlybot").

do(From, Args) ->
    case Args of
	[Room] ->
	    gen_event:notify(manager, {leave, Room, ?NICK});
	[Room, Nick] ->
	    gen_event:notify(manager, {leave, Room, Nick});
	[] ->
	    [Node, Domain, _] = From,
	    gen_event:notify(manager, {leave, Node ++ "@" ++ Domain, ?NICK})
    end,
    "".

info() ->
    {"leave", {?MODULE, do}, 0}.
