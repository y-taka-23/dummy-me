DummyMe - A simple dummy server for REST APIs
=============================================

[![Build Status](https://travis-ci.org/y-taka-23/dummy-me.svg?branch=master)](https://travis-ci.org/y-taka-23/dummy-me)

Installation
------------

Get the sources from the repository and build them by Stack.

```console
$ git clone git@github.com:y-taka-23/dummy-me.git
$ cd dummy-me
$ stack install
```

Quick Start
-----------

As an example, save the following as `db.json`.

```json
{
  "todos": [
    { "id": 1, "title": "Install Stack",   "completed": true },
    { "id": 2, "title": "Learn Haskell",   "completed": true },
    { "id": 3, "title": "Utilize DummyMe", "completed": false }
  ],
  "profile": { "name": "Alice", "email": "alice@example.com" }
}
```

Then launch DummyMe. It loads the contents of `db.json` and creates routes.

```console
$ dummy-me
  ____                                  __  __
 |  _ \ _   _ _ __ ___  _ __ ___  _   _|  \/  | ___
 | | | | | | | '_ ` _ \| '_ ` _ \| | | | |\/| |/ _ \
 | |_| | |_| | | | | | | | | | | | |_| | |  | |  __/
 |____/ \__,_|_| |_| |_|_| |_| |_|\__, |_|  |_|\___|
 _________________________________|___/_____________

 Laoding database from db.json

 - http://localhost:8080/todos
 - http://localhost:8080/profile
```

Open another terminal and try to access the dummy endpoints, e.g.

```console
$ curl http://localhost:8080/todos
[
  {
    "completed": true,
    "id": 1,
    "title": "Install Stack"
  },
  {
    "completed": true,
    "id": 2,
    "title": "Learn Haskell"
  },
  {
    "completed": false,
    "id": 3,
    "title": "Utilize DummyMe"
  }
]

$ curl -X PATCH -d '{ "completed": true }' http://localhost:8080/todos/3
$ curl http://localhost:8080/todos/3
{
  "completed" :true,
  "id": 3,
  "title": "Utilize DummyMe"
}
```

Now it's time to utilize DummyMe for creating your awesome web application. Enjoy!

### Routes

#### Plural routes

Arrays in `db.json` correspond to plural routes,
which support the following endpoints/verbs.

```
GET    /todos
GET    /todos/1
POST   /todos
PUT    /todos/1
DELETE /todos/1
PATCH  /todos/1
```

#### Singular routes

```
GET    /profile
PUT    /profile
PATCH  /profile
```

#### Administration

```
GET    /_db        # returns the whole current data in database
POST   /_snapshot  # dumps the data as a JSON file
```

CLI Options
-----------

```console
Usage: dummy-me [-f|--file string] [-p|--port int] [--quiet]
                [--snapshots string] [--version]
  A simple dummy server for REST APIs

Available options:
  -h,--help                Show this help text
  -f,--file string         JSON file which prescribes the initial
                           data (default: "db.json")
  -p,--port int            Port for the REST endpoints (default: 8080)
  --quiet                  Suppress log messages
  --snapshots string       Directory to store the database
                           snapshots (default: ".")
  --version                Show the version information
```

License
-------

This project is released under the BSD 3-clause license. For more details, see [LICENSE](./LICENSE) file.
