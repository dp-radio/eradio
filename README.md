# eradio

An Erlang HTTP server which streams data to multiple clients, lossily time-synchronized with low latency.

HTTP/1 and HTTP/2 use TCP as their transport protocol, which is problematic for "live streaming" (i.e. low-latency
time-synchronized streaming). Since TCP is a lossless protocol usually optimized for high throughput by default, the
operating system usually maintains a large buffer of unacknowledged outgoing data. This buffering may induce latency on
unreliable network connections. Furthermore, some popular browser HTTP media streaming implementations don't allow
efficiently seeking past bursts of buffered data to keep playback as close to the most recent data as possible.

`eradio` uses a couple techniques to avoid this TCP-induced latency. The first is adjusting the send buffer size. When
data is sent by an application using an ordinary Socket API, it must be buffered by the OS until the receiver
acknowledges receipt of the data. This is called the "send buffer", and `eradio` tries to minimize its capacity. Second,
since some operating systems such as Linux also maintain separate buffers preceding the send buffer which are not
application-controllable, `eradio` tries to query the current amount of data in these buffers before sending and stops
sending until the buffers have emptied. This technique has been analyzed in ["Low-Latency Adaptive Streaming over
TCP"](https://www.eecg.utoronto.ca/~ashvin/publications/low-latency-tomccap08.pdf) by Goel et. al.

## Development

[Internal API Documentation](https://dp-radio.github.io/eradio/)  
[Integration Test Reports (`common_test`)](https://dp-radio.github.io/eradio/ct/)

## Building with docker

Building with the [`container-build`](https://github.com/container-build/container-build) script requires python3 and
docker, and builds the project within the [`erlang:latest`](https://hub.docker.com/_/erlang/) Docker image.

```
$ scripts/container-build make
```

## Building without docker
#### Prerequisites:
- GNU Make
- Erlang >= 23
- `rebar3`
- `tsc` (Typescript compiler)

```
$ make all
```

The compiled Typescript is checked into source control at [`apps/eradio/priv/htdocs/js/`](apps/eradio/priv/htdocs/js/),
so it only needs to be compiled if it has changed. To compile only the Typescript code:

```
$ make ts
```

To compile only the Erlang and C code:

```
$ rebar3 compile
```

## Running with docker

Firstly, ensure that the Erlang OTP version used to build the project closely matches that of the
[`erlang:latest`](https://hub.docker.com/_/erlang/) image, which is the image the included [`Dockerfile`](Dockerfile)
uses.

To run the server in the background:

```
$ container=$(docker run -p 8080:8080 --detach $(docker build -q .))
```

To start a remote Erlang shell on the node:

```
$ docker exec -it $container erl -remsh eradio@localhost -hidden
```

To re-compile and hot-load code changes to modules on the running node (the `eradio_api_handler` module in this
example):

```
$ docker cp apps $container:/home/erlang/
$ docker exec -u root:root $container chown -R erlang:erlang /home/erlang/apps
$ docker exec -it $container erl -remsh eradio@localhost -hidden
(eradio@localhost)1> l(eradio_api_handler).
```

Only hot-load code when you know the new code will be compatible with any running state in the system.

## Running without docker
#### Prerequisites:
- Erlang (same version as built with)

```
$ ERL_LIBS=_build/default/lib erl -config config/sys.config -s eradio_app
```

The `_build/default/lib` directory contains the compiled `.beam` and files necessary to run `eradio`. However, `rebar3`
symlinks `_build/default/lib/eradio/priv` to the [`apps/eradio/priv/`](apps/eradio/priv) directory, which contains the
compiled `eradio.so`, along with the default [`index.html`](apps/eradio/priv/htdocs/index.html) and associated
Javascript. The `eradio.so` library contains the Erlang NIFs necessary for maintaining lower listener stream latency,
but it is not required.

`eradio` may be configured using `config/sys.config`, which is read at startup. The configuration keys are:

- `listen_ip` (optional): tuple containing the IP to listen on, as an IPv4 address in octets, or an IPv6 address in
  16-bit integers (`inet:ip_address()`). Listens on all interfaces if omitted.
- `listen_port` (default `8080`): integer port to listen on for HTTP connections.
- `sndbuf` (default `8000`): positive integer number of bytes to set as the send buffer size on listener streams. This affects their maximum latency.
- `webroot` (optional): string path to a directory containing files to be served as requested over HTTP. Defaults to
  [`$ERL_LIBS/eradio/priv/htdocs`](apps/eradio/priv/htdocs).

By default, `$ERL_LIBS/eradio/priv/source` is executed to provide source data on its standard output to broadcast to all
listeners. The source data must be sent in frames, each preceded by a 32-bit big-endian frame length. Note that since
frames of source data can be dropped depending on network conditions, the source data itself should be framed such that
listeners can be recover after missing frames. The first byte of each frame's data specifies the frame's type:
- Frame type 1: log message. The first byte specifies the log level, and the rest is the log message.
  - Log Level 1: error
  - Log Level 2: warning
  - Log Level 3: info
  - Log Level 4: debug
- Frame type 2: one frame of stream data for broadcast.
- Frame type 3: stream started notification. Additional metadata about the stream may optionally be present:
  - URI prefixed by its 16-bit big-endian length.
  - Name prefixed by its 16-bit big-endian length.
- Frame type 4: stream stopped notification.

### Using the Erlang remote shell

`eradio` must be started with the `-name`/`-sname` flag to enable Erlang dist. Note that connecting to the dist port
allows running arbitrary Erlang code. To listen only on the loopback interface, with the node name `'eradio@localhost'`:

```
$ ERL_LIBS=_build/default/lib erl -config config/sys.config -sname eradio@localhost -kernel inet_dist_use_interface '{127,0,0,1}' -s eradio_app
```

Then, to connect to the `'eradio@localhost'` node with an Erlang remote shell:

```
$ erl -remsh eradio@localhost -hidden
```

Note that when starting the Erlang remote shell, `$HOME/.erlang.cookie` must match that which `eradio` was started with.

### Reloading code

When connected to the Erlang remote shell, the `gen_cluster_code` module may be used as a convenience to hot-reload
modified code in `.beam` files:

```
(eradio@localhost)1> gen_cluster_code:load_all().
{ok,[eradio_app, eradio_server]}
```

Note that existing internal state is currently not guaranteed to be compatible with the newly loaded code, which may
cause crashes or strange behaviour.

## API

The HTTP server exposes an endpoint to listen for stream data, a REST API to fetch metadata about the stream, and a
websocket to listen for changes to the stream metadata. Currently, the best way to use this API is by using or
referencing [`api.ts`](apps/eradio/ts/modules/eradio/api.ts).
