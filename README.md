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
  "posts": [
    { "id": 1, "title": "json-server", "author": "typicode" }
  ],
  "comments": [
    { "id": 1, "body": "some comment", "postId": 1 }
  ],
  "profile": { "name": "typicode" }
}
```

License
-------

This project is released under the BSD 3-clause license. For more details, see [LICENSE](./LICENSE) file.
