-module(child).
-extends(parent).
-export([foo/0]).

foo() ->
    io:format("child:foo/0 ~n", []).

