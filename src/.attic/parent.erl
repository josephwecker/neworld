-module(parent).
-export([foo/0, bar/0]).

foo() ->
    io:format("parent:foo/0 ~n", []).

bar() ->
    io:format("parent:bar/0 ~n", []).

