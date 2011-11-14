-module(join).
-export([do/2, info/0]).
-define(NICK, "erlybot").

do(_From, Args) ->
    case Args of
	[Room] ->
	    gen_event:notify(manager, {join, Room, ?NICK}),
	    "вхожу в " ++ Room;
	[Room, Nick] ->
	    gen_event:notify(manager, {join, Room, Nick}),
	    "вхожу в " ++ Room ++ " под ником" ++ Nick;
	_ ->
	    "неверные параметры"
    end.

info() ->
    {"join", {?MODULE, do}, 0}.
