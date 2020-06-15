-module(test).

-export([start/2]).

start([H|T], File) ->
    mutex:executeFunction([H|T], File).
