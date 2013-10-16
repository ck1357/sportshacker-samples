%% http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/
%% erlc map_http_client.erl
%% erl -noshell -s map_http_client start -s init stop

-module(map_http_client).
-export([start/0]).

-define(SAMPLE_URLS, ["http://www.bbc.co.uk/news/",
		      "http://www.foxnews.com/",
		      "http://aljazeera.com/",
		      "http://www.cnn.com/",
		      "http://www.cnbc.com/",
		      "http://www.bloomberg.com/"]).

fetch_http(Url) -> 
    {ok, {_, _, Body}} = httpc:request(get, {Url, []}, [], []), 
    {Url, length(Body)}.

start() ->
    application:start(inets),
    io:format("~p~n", [lists:map(fun(Url) -> fetch_http(Url) end,
				 ?SAMPLE_URLS)]).



