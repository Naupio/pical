-module(simple_np_tcp).

-export([main_test/0]).

main_test() ->
    simple_tcp_example:spawn_conn(5).