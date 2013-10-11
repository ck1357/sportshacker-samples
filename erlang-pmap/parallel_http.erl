%% Joe Armstrong > Programming Erlang > p368
%% erlc parallel_http.erl
%% erl -noshell -s parallel_http start -s init stop

-module(parallel_http).
-export([start/0]).

pmap(F, L) ->
    S = self(),
    lists:map(fun(I) -> 
		      spawn(fun() -> pmap_f(S, F, I) end) 
	      end, L),
    pmap_gather(length(L) ,[]).

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

pmap_gather(0, L) ->
    L;
pmap_gather(N, L) ->
    receive
        {_Pid, Ret} -> pmap_gather(N-1, [Ret|L])
    end.

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



