---
layout: default
title: Getting Started
---

## Adding Scalamachine to your Project

Select the package for your host framework and add it to your project's build definition. If you decide later to change frameworks its a minimal change with respect to Scalamachine.

{% highlight scala %}
// Netty/Finagle
"com.github.jrwest" %% "scalamachine-netty" % "0.0.0-SNAPSHOT" 
// Lift
"com.github.jrwest" %% "scalamachine-lift" % "0.0.0-SNAPSHOT" 
{% endhighlight %}

*Note: The rest of this guide will use finagle as an example. You can find specific information about your framework on this page (page doesn't yet exist).*

## Hello, Scalamachine

Using Scalamachine in your application requires two things: Resources, implementations of the `Resource` trait and a `DispatchTable`. The simplest resource your application can implement is:

{% highlight scala %}
import com.github.jrwest.scalamachine.core.Resource
class MyResource extends Resource
{% endhighlight %}

A request handled by Scalamachine is run through a [decision flow](http://wiki.basho.com/images/http-headers-status-v3.png). Each decision queries either one or both of the request/response data and the resource to determine whether to continue or return a response. The simple resource above uses Scalamachine's default values. An application has many resources, so in order to detmermine which resource a request is for an application provides one or more `DispatchTable`s, which bind route's to resources. Add the following dispatch table to your application to serve the resource above at `/example`.

{% highlight scala %}
import com.github.jrwest.scalamachine.finagle.FinagleWebmachineV3
import com.github.jrwest.scalamachine.core._
import dispatch._
import Route._

class MyDispatchTable extends FinagleWebmachineV3 {
  route {
    pathMatching { "example" } serve new MyResource
  }
}
{% endhighlight %}

The final detail is to plug the `DispatchTable` into the application.

{% highlight scala %}
object ExampleServer extends App {

  val server =
    ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("FinagleWebmachine")
      .build(new FinagleWebmachineService(MyDispatchTable))

}
{% endhighlight %}

Using sbt, this application can be run using `sbt run`. We can use cURL to check that everything is working: 


    $  curl -v localhost:8080/example
    * About to connect() to localhost port 8080 (#0)
    *   Trying ::1... connected
    * Connected to localhost (::1) port 8080 (#0)
    > GET /empty HTTP/1.1
    > User-Agent: curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5
    > Host: localhost:8080
    > Accept: */*
    > 
    < HTTP/1.1 200 OK
    < Content-Type: text/html
    < Vary: 
    * no chunk, no close, no size. Assume close to signal end
    < 
    * Closing connection #0
    <html><body>Hello,Scalamachine</body></html>
    ```

## Hello, World

The resource in the previous example used all default values. The default resource responds only to `GET` requests that accept the `text/html` content-type. We can recreate these defaults in our own resource as a way to explore some of the functions that will be called on a resource during the handling of a request.

{% highlight scala %}
class MyResource extends Resource {
  import Resource.ContentTypesProvided 
  import Res._
  import HTTPMethods._

  override def allowedMethods(data: ReqRespData): (Res[List[HTTPMethod]],ReqRespData) = {
   (GET :: Nil, data)
  }

  override def contentTypesProvided(data: ReqRespData): (Res[ContentTypesProvided],ReqRespData) = {
    val provided = ((ContentType("text/html") -> createHtml(_: ReqRespData)) :: Nil
    (provided, data)
  }

  def createHtml(data: ReqRespData): (Res[Array[Byte]], ReqRespData) = {
    (result("<html><body>Hello, World!</body></html>".getBytes), data)
  }
}
{% endhighlight %}

We are still leveraging a lot of other default values as well but this resource will work just like the other's except for the HTML returned. `allowedMethods` specifies which HTTP methods this resource supports. The `contentTypeProvided` function is used in content negatiation. The returned list contains a list of content-type and body-rendering function pairs. 

Check out the [Resources Page](https://github.com/jrwest/scalamachine/wiki/resources) for more on what functions can be implemented. 


