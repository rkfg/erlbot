-module(title).
-export([do/2, info/0]).

do(_From, _Args) ->
    {_, Title} = get_title(),
    decode_string(Title).

decode_string(Str) ->
    io:format("~w~n", [Str]),
    case re:run(Str, <<"[À-ÿ]{3}">>, [unicode]) of
	{match, _} ->
	    %%Latin = mbcs:encode(Str, cp1252),
	    Result = case mbcs:decode(Str, cp1251) of
			 {error, _, _} ->
			     Str;
			 {error, _} ->
			     Str;
		         Decoded ->
			     Decoded
		     end,
	    Result;
	nomatch ->
	    %% try double-utf8
	    UniStr = try list_to_binary(Str) of
			 R ->
			     R
		     catch
			 _:_ ->
			     Str
		     end,
	    case mbcs:decode(UniStr, utf8) of
		{error, _, _} ->
		    Str;
		{error, _} ->
		    Str;
		Result ->
		    Result
	    end
    end.

append_cur_peak(Data, {Cur, Peak}) ->
    {DataCur, _} = get_field(Data, "Current Listeners:"),
    {DataPeak, NewData} = get_field(Data, "Peak Listeners:"),
    case DataCur of
        null ->
            NewCur = Cur;
        _ ->
            NewCur = Cur ++ [DataCur]
    end,
    case DataPeak of
        null ->
            NewPeak = Peak;
        _ ->
            NewPeak = Peak ++ [DataPeak]
    end,
    {NewCur, NewPeak, NewData}.

append_listeners([], {Cur, Peak}) ->
    NewCur = string:join(Cur, "+"),
    NewPeak = string:join(Peak, "+"),
    NewCur ++ "/" ++ NewPeak;

append_listeners(Data, {Cur, Peak}) ->
    {NewCur, NewPeak, NewData} = append_cur_peak(Data, {Cur, Peak}),
    append_listeners(NewData, {NewCur, NewPeak}).

get_field([Fieldname, Fieldvalue|Rest], Field) ->
    Fieldlist = unicode:characters_to_list(Fieldname),
    case Fieldlist of
        Field ->
            {unicode:characters_to_list(Fieldvalue), Rest};
        _ ->
            get_field(Rest, Field)
    end;

get_field(_, Field) ->
    case Field of
        "Current Song:" ->
            {"[без названия]", []};
        _ ->
            {null, []}
    end.

get_title() ->
    case httpc:request(get, {"http://radioanon.ru:8000", []}, [], []) of
	{ok, {{_, 200, _}, _, Page}} ->
	    ParsedPage = mochiweb_html:parse(Page),
	    Mounts = mochiweb_xpath:execute("//div[@class='streamheader']//td/h3/text()", ParsedPage),
	    case Mounts of
		[] ->
		    Title = SongTitle = "Никто не вещает.";
		[_|_] ->
		    Data = mochiweb_xpath:execute("//div[@class='streamheader']/following-sibling::table//td/text()", ParsedPage),
		    {SongTitle, _} = get_field(Data, "Current Song:"),
		    Title = SongTitle ++ " [" ++ append_listeners(Data, {[], []}) ++ "]"
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
