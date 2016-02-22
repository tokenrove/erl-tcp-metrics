tcp_metrics
===========

Reads RTT information from the Linux kernel over netlink.  (The
information is updated every five seconds because there is presently
no mechanism to subscribe to these updates.)

Some code is derived from iproute2.  Correspondingly, this package is
licenced under the GPLv2.  See the file [COPYING](COPYING) for
details.

Build
-----

Requires [libnl](https://www.infradead.org/~tgr/libnl/).

    $ ./rebar3 compile

Usage
-----

Start the `tcp_metrics` application and call `tcp_metrics:lookup(Ip)`
or read straight from the ETS table created; the returned value is
either `undefined` or an integer in milliseconds.

You can filter by source address by setting `source_address` in the
application environment before the application is started; e.g.:

```
application:set_env(tcp_metrics, source_address, {127,0,0,1}).
```
