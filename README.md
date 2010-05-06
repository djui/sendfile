Erlang sendfile() linked-in driver
==================================

**sendfile** is a linked-in driver for the sendfile syscall.

Based on original driver from [yaws](http://yaws.hyber.org).

Building and Installing
-----------------------

sendfile is built with [rebar](http://bitbucket.org/basho/rebar/) and
we do expect `rebar` to be in the search `PATH`.  
If `rebar` can not be found in the search `PATH` it will be
automatically downloaded to `support/rebar` for local usage.
