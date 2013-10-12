%% http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/
%% erlc map_http_client.erl
%% erl -noshell -s map_http_client start -s init stop

-module(map_http_client).
-export([start/0]).

start() ->
    application:start(inets),
    GetHttp=fun(Url) -> {ok, {_, _, Body}} = httpc:request(get, {Url, []}, [], []), {Url, length(Body)} end,   
    SampleUrls=["http://www.bbc.co.uk/news/",
		"http://observer.theguardian.com/",
		"http://www.dailymail.co.uk/home/index.html",
		"http://www.thesun.co.uk/sol/homepage/",
		"http://www.mirror.co.uk/",
		"http://www.independent.co.uk/",
		"http://www.express.co.uk/"],
    io:format("~p~n", [lists:map(GetHttp, SampleUrls)]).



