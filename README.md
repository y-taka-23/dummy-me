DummyMe - A simple dummy server for REST APIs
=============================================

[![Build Status](https://travis-ci.org/y-taka-23/dummy-me.svg?branch=master)](https://travis-ci.org/y-taka-23/dummy-me)

Installation
------------

Get the sources from the repository and build them by Stack:

```
$ git clone git@github.com:y-taka-23/dummy-me.git
$ cd dummy-me
$ stack install
```

How to Use
----------

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

### Routes

#### Plural routes

```
GET    /todos
GET    /todos/1
POST   /todos
PUT    /todos/1
DELETE /todos/1
```

#### Singular routes

```
GET    /profile
PUT    /profile
```

License
-------

This project is released under the BSD 3-clause license. For more details, see [LICENSE](./LICENSE) file.
