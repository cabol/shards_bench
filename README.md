Shards Benchmark
================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the example with `shards`:

``` bash
$ make run_shards
```

With `mnesia`:

``` bash
$ make run_mnesia
```

With `ets`:

``` bash
$ make run_ets
```

If you want to make you own configurations, edit [./config/sys.config](./config/sys.config) file, and
you run:

``` bash
$ make run
```

## Some graphics using gnuplot

```bash
$ gnuplot -e 'set term png; set output "plot.png"; plot "log/bench.log"'
```
