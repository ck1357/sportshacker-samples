%% http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/
%% erlc map_test.erl
%% erl -noshell -s map_test start -s init stop

-module(map_test).
-export([start/0]).

start() ->
    DoubleIt=fun(X) -> 2 * X end,
    io:format("~w~n", [lists:map(DoubleIt, [1, 2, 3, 4, 5])]).



