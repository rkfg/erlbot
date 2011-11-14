-module(djcal).
-export([do/2, info/0]).

-define(ACCOUNT, "calendar@radioanon.ru").

date_parse(Date, IncludeDate) ->
    io:format("Parsing date ~ts~n", [Date]),
    case string:tokens(Date, "-T:.+") of
	[_Y, M, D, H, Mi, _Sec, _Ms, _TZH, _TZM] ->
	    if
		IncludeDate ->
		    lists:flatten(io_lib:format("~s-~s ~s:~s", [M, D, H, Mi]));
		true ->
		    lists:flatten(io_lib:format("~s:~s", [H, Mi]))
	    end;
	_ ->
	    "???"
    end.

parse_entry(Entry) ->
    [{xmlAttribute, startTime, _, _, _, _, _, _, DateS, _}] = xmerl_xpath:string("gd:when/@startTime", Entry),
    StartTime = date_parse(DateS, true),
    [{xmlAttribute, endTime, _, _, _, _, _, _, DateE, _}] = xmerl_xpath:string("gd:when/@endTime", Entry),
    EndTime = date_parse(DateE, false),
    Title = lists:foldl(fun(Elem, AccIn) ->
			{xmlText, _, _, _, Text, text} = Elem,
			AccIn ++ Text
		end, [], xmerl_xpath:string("title/text()", Entry)),
    "[" ++ StartTime ++ "-" ++ EndTime ++ "] " ++ Title.
	
do(_From, _Args) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    Time = lists:flatten(io_lib:format("~w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w", [Y, M, D, H, Mi, S])),
    Url = "http://www.google.com/calendar/feeds/" ++ unicode:characters_to_list(re:replace(?ACCOUNT, "@", "%40")) ++ "/public/full?orderby=starttime&start-min=" ++ Time ++ "&sortorder=a&ctz=Europe%2fMoscow&singleevents=true",
    io:format("Teh url: ~ts~n", [Url]),
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {Xml, _Rest} = xmerl_scan:string(Body),
	    Entries = xmerl_xpath:string("//entry", Xml),
	    Titles = [parse_entry(Entry) || Entry <- Entries],
	    string:join(lists:sublist(Titles, 1, 5), "\n");
	{error, Reason} ->
	    "Ошибочка вышла: " ++ Reason
    end.

info() ->
    {["dj", "во"], {?MODULE, do}, 10}.
