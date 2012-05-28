---
layout: default
title: Dispatching
---

# Dispatching

[Resources](https://github.com/jrwest/scalamachine/wiki/resources) are not tightly-coupled to the URI that they are served at. Instead, an application binds resources to URIs using a `DispatchTable`. A `DispatchTable` is a specialized partial function from URIs (called *Routes*) to Resources. `DispatchTable`s are specific to the host framework your application is running on. This is necessary because the `DispatchTable` is responsible for converting the host framework's HTTP representation to Scalamachine's and vice-versa. Switching between `DispatchTable` implementations is simply a matter of changing the name of the `DispatchTable` an application extends. 

## Routes

Dispatching binds a `Route`, a pattern describing a URI path, to a Resource. A `Route` is created using a list of `RoutePart`s. A `RoutePart` is either a `StringPart` or a `DataPart`. When a request is received, the path of the request is matched against the route by splitting the path into tokens seperated by the `/` character. Routes must either match a path *exactly* or match the *beginning* of a path. If required to be exact each token must match each corresponding `RoutePart` with no remaining tokens or `RoutePart`s. If only the beginning is required to match each token must still match, but, remaining tokens are allowed. A token matches a `StringPart` if its lowercase value is equal to the lowercase value of the string the paramterizes `StringPart`. A token always matches a `DataPart`. 

Tokens in the URI path that match `DataPart`s are stored in a field, `pathInfo`, in `ReqRespData`. `pathInfo` is a `Map[Symbol,String]`, where they keys are the `Symbol` values parameterizing the `DataPart` that matched the token and the values are the matched tokens. If there was a `DataPart('myData)` in a route that matched the token "somevalue", `pathInfo` with contain a `('myData -> "somevalue")` pair. 

## Guards

The original implementation of Webmachine supported the idea of "Guards" in its [dispatch implementation](http://wiki.basho.com/Webmachine-Dispatching.html). This is not yet supported in Scalamachine but there are plans to add it.

# Examples

These examples are ported directly from [Basho's Webmachine Documentation](http://wiki.basho.com/Webmachine-Dispatching.html)

<table class="table-striped" style="width: 100%">
  <thead>
    <tr>
      <td>Route</td>
      <td>URI Path</td>
      <td>dispPath</td>
      <td>path</td>
      <td>pathInfo</td>
      <td>pathTokens</td>      
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>routeMatching(StringPart("a") :: Nil)</td>
      <td>/a</td>
      <td>""</td>
      <td>/a</td>
      <td>Map()</td>      
      <td>Nil</td>      
    </tr>    
    <tr>
      <td>routeStartingWith(StringPart("a") :: Nil)</td>
      <td>/a</td>
      <td>""</td>
      <td>/a</td>
      <td>Map()</td>
      <td>Nil</td>
    </tr>
    <tr>
      <td>routeStartingWith(StringPart("a") :: Nil)</td>
      <td>/a/b/c</td>
      <td>"b/c"</td>
      <td>/a/b/c</td>
      <td>Map()</td>
      <td>"b" :: "c" :: Nil</td>
    </tr>
    <tr>
      <td>routeMatching(StringPart("a") :: DataPart('foo) :: Nil)</td>
      <td>/a/b</td>
      <td>""</td>
      <td>/a/b</td>
      <td>Map('foo -> "b")</td>
      <td>Nil</td>
    </tr>
    <tr>
      <td>routeStartingWith(StringPart("a") :: DataPart('foo) :: Nil)</td>
      <td>/a/b</td>
      <td>""</td>
      <td>/a/b</td>
      <td>Map('foo -> "b")</td>
      <td>Nil</td>
    </tr>
    <tr>
      <td>routeStartingWith(StringPart("a") :: DataPart('foo) :: Nil</td>
      <td>/a/b/c/d</td>
      <td>"c/d"</td>
      <td>/a/b/c/d</td>
      <td>Map('foo -> "b")</td>
      <td>"c" :: "d" :: Nil</td>
    </tr>    
  </tbody>
</table>
