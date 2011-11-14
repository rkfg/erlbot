-module(title).
-export([do/2, info/0]).

do(_From, _Args) ->
    {_, Title} = get_title(),
    decode_string(Title).

decode_string(Str) ->
    UniStr = unicode:characters_to_binary(Str),
    case re:run(UniStr, "[À-ÿ]{5}", [unicode]) of
	{match, _} ->
	    {ok, CD} = iconv:open("utf-8", "cp1251"),
	    {ok, Result} = iconv:conv(CD, UniStr),
	    iconv:close(CD),
	    Result;
	nomatch ->
	    UniStr
    end.

append_cur_peak(Data, Offset, Str) ->
    Cur = binary_to_list(lists:nth(Offset + 6, Data)),
    Peak = binary_to_list(lists:nth(Offset + 7, Data)),
    Str ++ Cur ++ "+" ++ Peak.

append_listeners(Data, [], Offset, Result) ->
    append_cur_peak(Data, Offset, Result);

append_listeners(Data, [_Mount|Tail], Offset, Result) ->
    append_listeners(Data, Tail, Offset + 9, append_cur_peak(Data, Offset, Result) ++ "/").

get_title() ->
    case httpc:request(get, {"http://radioanon.ru:8000", []}, [], []) of
	{ok, {{_, 200, _}, _, Page}} ->
	    ParsedPage = mochiweb_html:parse(Page),
	    Mounts = mochiweb_xpath:execute("//div[@class='streamheader']//td/h3/text()", ParsedPage),
	    case Mounts of
		[] ->
		    Title = SongTitle = "Никто не вещает.";
		[_|Tail] ->
		    Data = mochiweb_xpath:execute("//div[@class='streamheader']/following-sibling::table//td[@class='streamdata']/text()", ParsedPage),
		    SongTitle = unicode:characters_to_list(lists:nth(9, Data)),
		    Title = SongTitle ++ " [" ++ append_listeners(Data, Tail, 0, "") ++ "]"
	    end;
	_ ->
	    Title = SongTitle = "Не удалось получить данные."
    end,
    {SongTitle, Title}.

update_loop(LastTitle) ->
    receive
	reload ->
	    title:update_loop()
    after 10000 ->
	    {SongTitle, Title} = get_title(),
	    if
		LastTitle =/= SongTitle ->
		    lists:map(fun(Room) -> gen_event:notify(manager, {subject, Room, decode_string(Title)}) end, account:title_announce());
		true ->
		    ok
	    end,
	    update_loop(SongTitle)
    end.

info() ->
    case whereis(titleupdater) of
	undefined ->
	    ok;
	Pid ->
	    exit(Pid, reload),
	    unregister(titleupdater)
    end,
    register(titleupdater, spawn(fun() ->
					 update_loop("")
				 end)),
    {["s", "ы"], {?MODULE, do}, 10}.
