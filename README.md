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

## Monitoring statsd activity

Regardless from the version of the app you built, you can always monitor the data you push to statsd. If you are
on a machine where `statsd` is not installed, make sure to do so. For example, on Mac OS X:

```
brew install nodejs
git clone https://github.com/etsy/statsd.git
cd statsd
cat << EOF >> config.js
{
 port: 8125,
 mgmt_port: 8126,
 backends: [ "./backends/console" ]
}
EOF
```

Once you are fully setup, you can do:

```
ekg-statsd-example
node stats.js config.js
```

* Running the former will show DEBUG messages on stderr;
* Running the latter should spit out the data `statsd` "sees" on stdout.
