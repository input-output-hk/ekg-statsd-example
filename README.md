# ekg-statsd-example

This is a simple standalone app demonstrating the usage of EKG to collect metrics which are later sent to `statsd`.
It comes in two flavours: headless (which uses `ekg-core`) and `with-server` (which makes available a Web UI).

## Building headless version

```
stack install --fast --file-watch
```

## Building with-server version

```
stack install --fast --file-watch --flag ekg-statsd-example:with-server
```
