# Scalamachine

Scalamachine is a port of [Basho's Webmachine](http://github.com/basho/webmachine) to Scala. It is pluggable into different server frameworks (support for [Finagle](http://github/twitter/finagle) and [Lift](http://liftweb.net) are in progress). 

## Getting Started 

To use Scalamachine select a host framework you want to run on -- don't fret it is easy to just switch out the dependency later if you change your mind. Add the dependency for that host framework to your project. 

Scalamachine is not yet published to any public repo so you will need to build and publish it locally:

```
sbt update publish-local
```	     

Finagle: 

```scala
"com.github.jrwest" %% "scalamachine-netty" % "0.0.0-SNAPSHOT" 
```

Lift:
```scala
"com.github.jrwest" %% "scalamachine-lift" % "0.0.0-SNAPSHOT" 
```

## Basics

### Resources

At the core, Scalamachine works by running a `Resource`, which describes your API resource, through a [decision flow](http://wiki.basho.com/images/http-headers-status-v3.png). At each decision in the flow, the resource or the request/response data is queried to determine how to proceed, resulting in either another decision or the termination of the flow -- and therefore a response to the request. The functions called on your `Resource`s are all of the signature `ReqRespData => (Res[T],ReqRespData)`. ReqRespData is the class Scalamachine uses to represent both request and response data. 

The simplest resource you can create is:

```scala
class MyResource extends Resource
```

Scalamachine includes defaults for every function. Here is that same resource more explicitly: 

```scala

class MyResource extends Resource {
  import Resource.ContentTypesProvided 
  import Res._ 

  override def allowedMethods(data: ReqRespData): (Res[List[HTTPMethod]],ReqRespData) = {
   (GET :: Nil, data)
  }

  override def contentTypesProvided(data: ReqRespData): (Res[ContentTypesProvided],ReqRespData) = {
    val provided = ((ContentType("text/html") -> createHtml(_: ReqRespData)) :: Nil
    (provided, data)
  }

  def createHtml(data: ReqRespData): (Res[Array[Byte]], ReqRespData) = {
    result("<html><body>Hello, World!</body></html>".getBytes, data)
  }
}
```

We are still leveraging a lot of default values but this basic resource will respond only to GET requests (returning 406 Method Not Allow in other cases) and only to those GET requests that accept the "text/html" content type. 

### Dispatch

In your application, you will define many Scalamachine `Resource`s. In order for Scalamachine to know which requests are bound for which resources, you must add routes to a `DispatchTable`. There are specific `DispatchTable` implementions for each host framework, however, adding routes to them is the same.

If you are using Finagle, `object MyDispatchTable extends FinagleWebmachineV3`. If you are using Lift, `object MyDispatchTable extends LiftWebmachineV3`.

An example `DispatchTable`:

```scala
object MyDispatchTable extends FinagleWebmachineV3 {
  addRoute {
    routeMatching(StringPart("resources") :: DataPart('id) :: Nil, new MyResource)
  }       

  addRoute {
    routeStartingWith(StringPart("other_resources") :: Nil, new OtherResource)
  }
}
```

Routes are added to the dispatch table using `addRoute` and either `routeMatching` or `routeStartingWith`. When created by `routeMatching`, request's with a url path matching the route exactly are the only ones dispatched to the resource. With `routeStartingWith` requests whose path match exactly or whose prefix matches exactly will be routed to the resource. Each route is comrpised of parts that are used to determine if the route matches. A `StringPart` matches the corresponding path part (where parts are the list of strings resulting from splitting the path on the "/" character) if the string is exactly equal to the part, ignoring case. A DataPart always matches its corresponding path part. For example, the first route above would match "/resources/1" but not "/resources/1/sub_resources" or "/resources". The second route would match "/other_resources/1" and "/other_resources/2/something" and "/other_resources".

### Getting Things Running

Once you have a `DispatchTable` for your host framework you can plug it in and run your server. The instructions are specific to each host framework.

For finagle a `Service` and `Filter` are provided so you can build a `Service` just using Scalamachine or plug Scalamachine into an existing Finagle application. The example application below uses the `Service` Scalamachine provides to build a simple finagle HTTP server:

```scala
object ExampleServer extends App {

  val server =
    ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("FinagleWebmachine")
      .build(new FinagleWebmachineService(MyDispatchTable))

}
```


For lift, you append your `DispatchTable` to the `statelessRewrite` rules in `Bool.scala`:

```scala
LiftRules.statelessDispatch.append(ScalamachineExample)
```

# License
Copyright © 2012 Jordan West

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.






