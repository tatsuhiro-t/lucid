Lucid - HTTP/2 server in Erlang
===============================

Lucid is HTTP/2 server written in Erlang.
This project was started primarily for the author to learn Erlang/OTP.

Current Status
--------------

Currently, Lucid implements HTTP/2 draft version `h2-14
<http://tools.ietf.org/html/draft-ietf-httpbis-http2-14>`_ protocol
specification.

Flow control, CONTINUATION, padding and HPACK have been implemented.
Lucid works with Firefox nightly with
``network.http.spdy.enforce-tls-profile`` set to ``false``.
This is because Erlang lacks AEAD cipher suites.

Lucid also works fine with nghttp client from `nghttp2 project
<https://nghttp2.org>`_.

Test server is now up at https://nghttp2.org:3456/

Requirements
------------

Lucid is written in purely Erlang only.
Erlang/OTP 17.1 is required.
Older version may work, but we have not tested.

How To Use
----------

::

    $ erl -pa ebin
    1> make:all([load]).
    2> application:start(lucid).

Now server listens to port 3000.

To enable SSL/TLS, edit *ebin/lucid.app* file and set ``ssl``
environment variable to ``true`` and specify ``certfile`` and
``keyfile`` to the path to server's certificate file and private key
respectively::

    ...
    {ssl, true},
    {ssl_options, [{certfile, "server.crt"},
                   {keyfile, "server.key"},
    ...

ssl application must be started before lucid::

    $ erl -pa ebin
    1> make:all([load]).
    2> ssl:start().
    3> application:start(lucid).

When accessed, server returns welcome page with request headers.  To
serve static file from the current working directory, replace the call
``handle_request`` to ``handle_request_file``::

    handle_cast(request, State) ->
        case handle_request_file(State) of
            stop ->
                {stop, normal, State};
            State2 ->
                {noreply, State2}
        end;
