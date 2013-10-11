%% erlc parallel_http.erl
%% erl -noshell -s parallel_http start -s init stop

-module(parallel_http).
-export([start/0]).

pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]		    
    end;
pmap_gather([]) ->
    [].

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
    io:format("~p~n", [pmap(GetHttp, SampleUrls)]).



