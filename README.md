# Jobs Service

[![License: MIT][MIT Badge]][MIT]
[![GitHub Release Badge]][GitHub Releases]

An HTTP service for converting JSON based tasks.

## Usage

You can use [Docker] to run the service without other dependencies needed:

```
$ docker run --name jobs --rm -it -p 8080:8080 aialferov/jobs run
```

After you have run the service you get into the service console. There you can
run commands to get some information about the service, for example about API it
provides or the API usage example.

### API

The service exposes the following API:

```
POST /api/v1/jobs[?tasks=<Command>]
<Tasks>

Commands:
    flatten    sort given tasks in accordance with dependencies specification
    scriptize  same as flatten but make an executable shell script of it
```

Where "Tasks" is a JSON with the following format:

```
{
  "tasks":[
    {
      "name":"<Task Name>",
      "command":"<Shell Command>"
      "requires":[ # optional
        "<Another Task Name>"
        ...
      ]
    },
    ...
   ]
}
```

#### Example

Given the following example JSON:

```
$ cat example.json
{
  "tasks":[
    {
      "name":"task-1",
      "command":"cat /tmp/file1",
      "requires":[
        "task-2"
      ]
    },
    {
      "name":"task-2",
      "command":"echo 'Hello World!' > /tmp/file1",
    }
  ]
}
```

We expect the "task-2" happens before the "task-1". Check it with flattening
first:

```
$ curl -sd @example.json localhost:8080/api/v1/jobs?tasks=flatten | jq
[
  {
    "command": "echo 'Hello World!' > /tmp/file1",
    "name": "task-2"
  },
  {
    "command": "cat /tmp/file1",
    "name": "task-1"
  }
]
```

And then create an executable script:

```
$ curl -sd @example.json localhost:8080/api/v1/jobs?tasks=scriptize
#!/bin/bash

echo 'Hello World!' > /tmp/file1
cat /tmp/file1
```

### Without Docker

You will need [Erlang] installed in your system to build and run the service.

To build, run unit tests and run:

```
$ make
$ make check
$ make run
```

The latter runs executable that is located in "_build/default/bin" after build
and could be run directly:

```
$ _build/default/bin/jobs
```

Make targets provide many other functions for development and packaging cycle
including getting into an Erlang shell, joining a running service instance
Erlang VM, Docker image related operations and others.

Run "make usage" and "make docker-usage" to see them all.

### Kubernetes

To run the service as a [Kubernetes] deployment, just create it from the
manifest:

```
$ kubectl create -f https://raw.githubusercontent.com/aialferov/jobs/master/manifests/jobs.yaml
```

When the pod is running you can attach to the service console:

```
$ POD=$(kubectl get po -l run=jobs -o jsonpath={.items[*].metadata.name})
$ kubectl attach -it $POD
```

You have to close the window as there is no other way to detach.

### Make Interface

[Make] interface is provided for operational purposes and is based on the [Mk]
project. Please refer the [Erlangbin.mk] and [Docker.mk] makefiles description
for details.

<!-- Links -->

[MIT]: https://opensource.org/licenses/MIT
[GitHub Releases]: https://github.com/aialferov/jobs/releases

[Mk]: https://github.com/aialferov/mk
[Make]: https://www.gnu.org/software/make
[Docker]: https://docs.docker.io
[Erlang]: http://erlang.org
[R3tmpl]: https://github.com/aialferov/r3tmpl
[Kubernetes]: https://kubernetes.io
[Docker.mk]: https://github.com/aialferov/mk#dockermk
[Erlangbin.mk]: https://github.com/aialferov/mk#erlangbinmk

<!-- Badges -->

[MIT Badge]: https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square
[GitHub Release Badge]: https://img.shields.io/github/release/aialferov/jobs/all.svg?style=flat-square
 
