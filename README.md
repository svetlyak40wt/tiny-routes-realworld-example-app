# ![CL tiny-routes RealWorld Example App](logo.png)

### [tiny-routes](https://github.com/jeko2000/tiny-routes) codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://demo.realworld.io/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **[tiny-routes](https://github.com/jeko2000/tiny-routes)** including CRUD operations, authentication, routing, pagination, and more.

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
Enjoy!
