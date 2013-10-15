%% Joe Armstrong > Programming Erlang > p368
%% erlc pmap_http_client.erl
%% erl -noshell -s pmap_http_client start -s init stop

-module(pmap_http_client).
-export([start/0]).

-define(SAMPLE_URLS, ["http://www.bbc.co.uk/news/",
		      "http://www.foxnews.com/",
		      "http://aljazeera.com/",
		      "http://www.cnn.com/",
		      "http://www.cnbc.com/",
		      "http://www.bloomberg.com/"]).

pmap(F, L) ->
    S = self(),
    lists:map(fun(I) -> 
		      spawn(fun() -> pmap_f(S, F, I) end) 
	      end, L),
    pmap_gather(length(L) ,[]).

pmap_f(Parent, F, I) ->
    S=self(),
    io:format("Fetching ~p (Pid ~p)~n", [I, S]),
    Parent ! {self(), (catch F(I))}.

pmap_gather(0, L) ->
    lists:reverse(L);
pmap_gather(N, L) ->
    receive
        {Pid, Ret} -> 
	    io:format("Received ~p (pid ~p)~n", [Ret, Pid]),
	    pmap_gather(N-1, [Ret|L])
    end.

start() ->
    application:start(inets),
    GetHttp=fun(Url) -> 
		    {ok, {_, _, Body}} = httpc:request(get, {Url, []}, [], []), 
		    {Url, length(Body)} 
	    end,   
    io:format("~p~n", [pmap(GetHttp, ?SAMPLE_URLS)]).



