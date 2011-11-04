%% Copyright ProcessOne 2006-2010, eurekafag 2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>, eurekafag <eurekafag@eureka7.ru>

-module(jbot).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-behavior(gen_event).
-export([start/0, start/2, join_room/4, leave_room/4, loop/0, session_link/2, talker/1, talker_link/0, emptyloop/0]).
-export([init/1, handle_event/2, code_change/3, handle_call/2, handle_info/2, terminate/2]).

-define(LOG(M), io:format(M)).
-define(LOG(M, A), io:format(M, A)).

init([JID, Password]) ->
    {ok, TalkerPID} = supervisor:start_child(jbsup, {talkerid, {?MODULE, talker_link, []}, permanent, 5000, worker, [jbot]}),
    {ok, Self} = supervisor:start_child(jbsup, {coreid, {?MODULE, session_link, [JID, Password]}, permanent, 5000, worker, [jbot]}),
    ?LOG("Self = ~w, TalkerPID = ~w~n", [Self, TalkerPID]),
    {ok, {false, [], TalkerPID, Self}}.

start() ->
    {JID, Password} = account:creds(),
    start(JID, Password).

start(JID, Password) ->
    application:start(exmpp),
    gen_event:start({local, manager}),
    ?LOG("Supervisor status: ~p~n", [supervisor:start_link({local, jbsup}, jbot_sup, [])]),
    gen_event:add_handler(manager, ?MODULE, [JID, Password]),
    emptyloop().

emptyloop() ->
    timer:sleep(1000),
    ?MODULE:emptyloop().

session_link(JID, Password) ->
    Self = spawn_link(fun() -> session(JID, Password) end),
    gen_event:notify(manager, {selfpid, Self}),
    {ok, Self}.

session(JID, Password) ->
    %%timer:sleep(1000),
    MySession = exmpp_session:start({1, 0}),
    gen_event:notify(manager, {talkersession, MySession}),
    %% Create XMPP ID (Session Key):
    [User, Server] = string:tokens(JID, "@"),
    MyJID = case string:tokens(Server, "/") of
		[NewServer, Resource] ->
		    ConnServer = NewServer,
		    exmpp_jid:make(User, NewServer, Resource);
		_ ->	    
		    ConnServer = Server,
		    exmpp_jid:make(User, Server, random)
	    end,

    io:format("MyJID: ~p~n", [MyJID]),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    exmpp_session:connect_TCP(MySession, ConnServer, 5222, [{whitespace_ping, 30}]),
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession, "DIGEST-MD5")
    catch
	throw:{auth_error, 'not-authorized'} ->
	    halt(1)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Echo Ready")),
    join_room(MySession, "erlytest", "conference.jabber.ru", "erlybot"),
    gen_event:notify(manager, load_commands),
    loop().

load_command([Com | Rest], Result) ->
    case code:is_loaded(Com) of
	{file, _} ->
	    code:purge(Com);
    	_ ->
    	    ok
    end,
    code:load_file(Com),
    {Commands, Function} = Com:info(),
    io:format("Processing ~p, Result is ~p~n", [{Commands, Function}, Result]),
    case Commands of
	[A|_] when is_list(A) ->
	    load_command(Rest, Result ++ [{Command, Function} || Command <- Commands]);
	_ ->
	    load_command(Rest, Result ++ [{Commands, Function}])
    end;

load_command([], Result) ->
    Result.

join_room(Session, Room, Server, Nick) ->
    exmpp_session:send_packet(Session, exmpp_stanza:set_recipient(exmpp_presence:available(), Room ++ "@" ++ Server ++ "/" ++ Nick)).
    
leave_room(Session, Room, Server, Nick) ->
    exmpp_session:send_packet(Session, exmpp_stanza:set_recipient(exmpp_presence:unavailable(), Room ++ "@" ++ Server ++ "/" ++ Nick)).

talker_link() ->
    TalkerPID = spawn_link(?MODULE, talker, [false]),
    gen_event:notify(manager, {talkerpid, TalkerPID}),
    {ok, TalkerPID}.

talker(Session) ->
    receive
	{say, To, Text} ->
	    if
		is_binary(Text) ->
		    io:format("Saying ~ts in ~ts~n", [unicode:characters_to_list(Text), To]);
		true ->
		    io:format("Saying ~ts in ~ts~n", [unicode:characters_to_list(list_to_binary(Text)), To])
	    end,
	    if
		Session =/= false ->
		    exmpp_session:send_packet(Session, exmpp_stanza:set_recipient(exmpp_message:groupchat(unicode:characters_to_binary(Text)), To));
		true ->
		    ok
	    end,
	    timer:sleep(2000),
	    talker(Session);
	upgrade ->
	    ?MODULE:talker(Session);
	{session, NewSession} ->
	    talker(NewSession);
	_ ->
	    talker(Session)
    after 2000 ->
	    talker(Session)
    end.
	
handle_event(stop, State = {Session, _Commands, _TalkerPID, _Self}) ->
    exmpp_session:stop(Session),
    {ok, State};

handle_event(reload, State = {_Session, _Commands, TalkerPID, Self}) ->
    code:purge(?MODULE),
    code:load_file(?MODULE),
    code:purge(modlist),
    code:load_file(modlist),
    TalkerPID ! upgrade,
    Self ! upgrade,
    {ok, State};

handle_event(load_commands, {Session, _Commands, TalkerPID, Self}) ->
    Mods = modlist:modules(),
    Commands = dict:from_list(load_command(Mods, [])),
    io:format("Commands: ~p, Modules: ~p~n", [Commands, Mods]),
    {ok, {Session, Commands, TalkerPID, Self}};

handle_event(Response = {say, _To, _Text}, State = {_Session, _Commands, TalkerPID, _Self}) ->
    TalkerPID ! Response,
    {ok, State};

handle_event({message, Packet}, State = {_Session, Commands, _TalkerPID, _Self}) ->
    RawMessage = exmpp_message:get_body(Packet#received_packet.raw_packet),
    io:format("Message: ~ts in ~w~n", [unicode:characters_to_list(RawMessage), self()]),
    Message = binary_to_list(RawMessage),
    case Message of
	"*" ++ Command ->
	    [Cmd | Args] = string:tokens(Command, " "),
	    case dict:find(Cmd, Commands) of
		{ok, Fun} ->
		    spawn(fun() -> responder(Fun, Args, [binary_to_list(X) || X <- tuple_to_list(Packet#received_packet.from)]) end);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    {ok, State};
		
handle_event({presence, Packet}, State = {Session, _Commands, _TalkerPID, _Self}) ->
    case exmpp_jid:make(_From = Packet#received_packet.from) of
	JID ->
	    case _Type = Packet#received_packet.type_attr of
		"available" ->
		    %% handle presence availabl
		    ok;
		"unavailable" ->
		    %% handle presence unavailable
		    ok;
		"subscribe" ->
		    presence_subscribed(Session, JID),
		    presence_subscribe(Session, JID);
		"subscribed" ->
		    presence_subscribed(Session, JID),
		    presence_subscribe(Session, JID)
	    end
    end,
    {ok, State};

handle_event({talkerpid, TalkerPID}, {Session, Commands, _OldTalkerPID, Self}) ->
    ?LOG("Setting talkerpid~n"),
    {ok, {Session, Commands, TalkerPID, Self}};

handle_event({selfpid, Self}, {Session, Commands, TalkerPID, _OldSelf}) ->
    ?LOG("Setting selfpid~n"),
    {ok, {Session, Commands, TalkerPID, Self}};

handle_event({talkersession, Session}, {_Session, Commands, TalkerPID, Self}) ->
    ?LOG("Setting session~n"),
    TalkerPID ! {session, Session},
    {ok, {Session, Commands, TalkerPID, Self}}.

%% Process exmpp packet:
loop() ->
    receive
	upgrade ->
	    ?MODULE:loop();

        Record = #received_packet{packet_type=message,
				  type_attr=Type} when Type =/= "error" ->
	    case exmpp_xml:get_element_by_ns(Record#received_packet.raw_packet, 'jabber:x:delay') of
		undefined ->
		    gen_event:notify(manager, {message, Record});
		_ ->
		    ok
	    end,
            loop();

	Record = #received_packet{packet_type=message,
				  type_attr=Type,
				  raw_packet=Raw} when Type == "error" ->
	    io:format("Received an error stanza:~n~p~n~n", [Record]),
	    To = exmpp_stanza:get_sender(Raw),
	    Body = exmpp_message:get_body(Raw),
	    gen_event:notify(manager, {say, To, Body}),
	    loop();

	Record when Record#received_packet.packet_type == 'presence' ->
	    gen_event:notify(manager, {presence, Record}),
	    loop();

        Record ->
            io:format("Received a stanza:~n~p~n~n", [Record]),
            loop()
    end.

responder(Fun, Args, FromJID) ->
    ?LOG("In responder: ~p, ~p, ~p~n", [Fun, Args, FromJID]),
    {M, F} = Fun,
    [Node, Domain, Resource] = FromJID,
    Recipient = Node ++ "@" ++ Domain,

    case M:F(FromJID, Args) of
	"" ->
	    ok;
	Reply ->	
	    Response = Resource ++ ", " ++ Reply,
	    gen_event:notify(manager, {say, Recipient, Response})
    end.


presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Request, State) ->
    {ok, Request, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.
