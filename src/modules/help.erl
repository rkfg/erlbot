-module(help).
-export([do/2, info/0]).

do(_From, _Args) ->
    "я — экспериментальный бот на Erlang. Умею не очень много, но стараюсь.\nИсходники: https://github.com/eurekafag/jbot".

info() ->
    {["help", "h", "р"], {?MODULE, do}, 10}.
