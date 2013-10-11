%% http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/
%% erlc http_client_test.erl
%% erl -noshell -s http_client_test start -s init stop

-module(http_client_test).
-export([start/0]).

start() ->
    application:start(inets),
    Url="http://www.bbc.co.uk/news",
    {ok, {{_HttpVersion, Code, _Msg}, _Headers, Body}} = httpc:request(get, {Url, []}, [], []),
    io:format("Code: ~w~n", [Code]),
    %% io:format("Headers: ~w~n", [Headers]).
    io:format("Body: ~w~n", [length(Body)]).   
