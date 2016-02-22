# resm

> Simple resource manager that provides resources on demand.

## Configuration
For service port (`service_port`) and resource pool size (`resource_num`) configuration you may edit
* `resm.config` file to set default service settings (provided in compile time)
* or `$RELEASE_ROOT/releases/$VSN/sys.config` to configure installed service

## `make` goals

Makefile is a *mostly* simple wrapper over [rebar3](https://github.com/rebar/rebar3) build script. Project build was tested on Ubuntu 14.04 and [Erlang 18.2 x64](https://www.erlang-solutions.com/resources/download.html) (but Erlang 17+ also be appropriate).

### `make compile`

It will download required deps and compile the project.

### `make test`

Unit tests run.

### `make shell`

Run service in `erl` shell for interactive development. Now you can make requests to [service](http://localhost:8888/list/):

```bash
> http http://localhost:8888/list/             
HTTP/1.1 200 OK
content-length: 48
content-type: application/json; charset=utf-8
date: Mon, 22 Feb 2016 18:00:16 GMT
server: Cowboy

{
    "allocated": {}, 
    "deallocated": [
        0, 
        1, 
        2, 
        3, 
        4, 
        5, 
        6, 
        7
    ]
}
```

### `make release`

Build application release (it will placed at `_build/prod/rel/`).

### `make docker-image`

Self described. Note that containerized service used port 8888 by default (edit `docker/Dockerfile` for change this). Now you can run docker container from created image:

```bash
> docker run -d -p 8888:8888 --name resm injecto/resm:1.0.0
...
> docker stop resm
```

Or just use `make docker-run` and `make docker-stop` for these purposes.

### `make deb`

Pack project into `deb`-package (it will placed at `_build/deb`). Now you can install and run service:

```bash
> sudo dpkg -i _build/deb/resm_1.0-0_amd64.deb
...
> sudo /etc/init.d/resm start
```

### `make clean`

Clean project tree.
