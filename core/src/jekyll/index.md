---
layout: default
title: Home
---

Scalamachine is a port of [Basho](http://basho.com)'s [Webmachine](http://github.com/basho/webmachine) to Scala. Pluggable into your choice of existing server-side web frameworks, it enables development of rich REST APIs that conform to HTTP semantics. Develop your API alongside your existing project or standalone. 

REST APIs revolve around the idea of resources. So do APIs built with Scalamachine. A resource in Scalamachine is an implementation of the `Resource` trait, a set of functions the implementation can override. The collection of functions define the properties of a resource and dictate how the resource can be interacted with from the outside world. For example, what content types, charsets and encodings are supported, the etag or last modified date of the resource, and how the request is authorized. Scalamachine puts all of these details upfront using them to do things like content negotiation and handling of conditional requests for you. Scalamachine implements the [V3 Webmachine Flow Diagram](http://wiki.basho.com/images/http-headers-status-v3.png) which defines the semantics of how each function is used. 

For each request Scalamachine must decide which resource it will run through the [flowchart](http://wiki.basho.com/images/http-headers-status-v3.png) to determine the result of the request. A `DispatchTable` defines routes, or url path patterns, and which resources are bound to requests matching those routes.

The project was just released open-sourced and published, more documentation will be added shortly.