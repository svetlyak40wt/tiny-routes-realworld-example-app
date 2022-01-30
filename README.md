# ![CL tiny-routes RealWorld Example App](logo.png)

### [tiny-routes](https://github.com/jeko2000/tiny-routes) codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://demo.realworld.io/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged backend application built with **[tiny-routes](https://github.com/jeko2000/tiny-routes)** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **[tiny-routes](https://github.com/jeko2000/tiny-routes)** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# Overview

This is [clack](https://github.com/fukamachi/clack) application built using [tiny-routes](https://github.com/jeko2000/tiny-routes) library.

# Installation
To install, first navigate to a directory where your [Quicklisp](https://www.quicklisp.org/beta/) installation expects local packages (e.g., ~/quicklisp/local-projects) and clone this repository as follows:

```bash
git clone https://github.com/jeko2000/tiny-routes-realworld-example-app
```
Then, start up your Common Lisp implementation, quick load the package, and start the server.
```bash
(ql:quickload :conduit)
(conduit:start-app)
```
# Testing
The server can be tested via the [realword Postman collection](https://github.com/gothinkster/realworld/blob/main/api/Conduit.postman_collection.json).

For reference, this is the response as of version 0.1.0.
```
  ┌─────────────────────────┬───────────────────┬───────────────────┐
  │                         │          executed │            failed │
  ├─────────────────────────┼───────────────────┼───────────────────┤
  │              iterations │                 1 │                 0 │
  ├─────────────────────────┼───────────────────┼───────────────────┤
  │                requests │                32 │                 0 │
  ├─────────────────────────┼───────────────────┼───────────────────┤
  │            test-scripts │                48 │                 0 │
  ├─────────────────────────┼───────────────────┼───────────────────┤
  │      prerequest-scripts │                18 │                 0 │
  ├─────────────────────────┼───────────────────┼───────────────────┤
  │              assertions │               331 │                 0 │
  ├─────────────────────────┴───────────────────┴───────────────────┤
  │ total run duration: 19.4s                                       │
  ├─────────────────────────────────────────────────────────────────┤
  │ total data received: 17.08kB (approx)                           │
  ├─────────────────────────────────────────────────────────────────┤
  │ average response time: 81ms [min: 5ms, max: 631ms, s.d.: 185ms] │
  └─────────────────────────────────────────────────────────────────┘
```
Enjoy!
