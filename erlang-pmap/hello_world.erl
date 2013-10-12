%% erlc hello_world.erl
%% erl -noshell -s hello_world start -s init stop

-module(hello_world).
-export([start/0]).

start() -> 
    io:format("~p~n", ["Hello World from Erlang!"]).
