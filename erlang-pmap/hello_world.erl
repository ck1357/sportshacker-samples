%% http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/
%% erl hello_world.erl
%% erl -noshell -s hello_world start -s init stop

-module(hello_world).
-export([start/0]).

start() ->
    io:format("Hello world from Erlang!~n").
