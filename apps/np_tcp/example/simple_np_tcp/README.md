simple_np_tcp
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell
    1> application:start(simple_np_tcp).
    2> simple_np_tcp:main_test().