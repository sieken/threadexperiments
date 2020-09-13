`jmalloc`: A `malloc()` for multi-threaded use in order to compare the performance when using _Thread Local Storage_ (TLS) for the free-list and when using a central locked free-list.

![TLS](img/tls.png)

`token`: A comparison between _OS threads_ (C) and _green threads_ (Erlang), where threads pass a token around in a loop.

![ErlangVsC](img/erlang-c.png)
![Erlang](img/erlang.png)
