%% http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/
%% http://montsamu.blogspot.co.uk/2007/02/erlang-parallel-map-and-parallel.html
%% erlc pmap_test.erl
%% erl -noshell -s pmap_test start -s init stop

-module(pmap_test).
-export([pmap/2, start/0]).

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
    DoubleIt=fun(X) -> 2 * X end,
    io:format("~w~n", [pmap(DoubleIt, [1, 2, 3, 4, 5])]).



