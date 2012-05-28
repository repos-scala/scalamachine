---
layout: default
title: Resources
---

# Resources

Resources are implementations of the `Resource` trait, overriding default definitions for a variety of functions, each having the signature `[A]ReqRespData => (Res[A],ReqRespData)`. 

#### `ReqRespData`

`ReqRespData` is an immutable structure containing information about the request and response such as the content bodies and headers. Functions may manipulate the data and return new instances. For more on what `ReqRespData` provides check out this page (does not exist yet).

#### Return Value

Each function returns a 2-tuple of a result, a `Res`, and a possibly new `ReqRespData`. The `ReqRespData` will be used as the processing continues -- until, it is yet again modified internally by Scalamachine or by another `Resource` function.

##### `Res[A]`

Any `Resource` function can either continue or halt the processing of a request implicitly, because of the actual value returned, or it can explicitly halt processing, by signaling that it wishes to do so. This is encoded via `Res[A]`, a `sealed trait` that can be one of: 

* `ValueRes[A]`
* `HaltRes` 
* `ErrorRes` 

`ValueRes[A]` boxes an actual value, `A`, and is similar to `Some[A]`. Depending on the returned value the processing of the request may continue or it may complete. `HaltRes` and `ErrorRes` are used to explicitly stop processing and both have type `Res[Nothing]` -- `Res[A]` is covariant*. `HaltRes` is used to stop processing with a specific response code. `ErrorRes` will always result in a  `500 Internal Server Error` and the response body with contain a specific value specified when creating an `ErrorRes`.

`ValueRes[A]`, `HaltRes`, and `ErrorRes` are all `case classes` so they can be instantiated using the default `apply` constructor. However it is suggested, to help the Scala type inferencer, that you import `Res._` and use the functions `result[A](a: A)`, `halt(code: Int)` and `error(e: Any)`, which have the less specific result type `Res[A]`. 

***

# Resource Functions


<table class="table-striped" style="width: 100%">
 <thead>
   <td><b>Function Name</b></td>
   <td><b>Res Type</b></td>
   <td><b>Description</b></td>
   <td><b>Default</b></td>
 </thead>
 <tbody>
   <tr>
     <td>serviceAvailable</td>
     <td>Boolean</td>
     <td>true if the service(s) necessary to respond to a request handled by this resource are available, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>knownMethods</td>
     <td>List[HTTPMethod]</td>
     <td>list of HTTP methods known by this resource</td>
     <td>List(OPTIONS, TRACE, CONNECT, HEAD, GET, POST, PUT, DELETE)</td>
   </tr>
   <tr>
     <td>uriTooLong</td>
     <td>Boolean</td>
     <td>true if request URI is too long, false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>allowedMethods</td>
     <td>List[HTTPMethod]</td>
     <td>list of HTTP methods allowed by this resource, if a request to this resource with made with a method other than those returned by this function a "406 Method Not Allowed" response is returned</td>
     <td>List(GET)</td>
   </tr>
   <tr>
     <td>isMalformed</td>
     <td>Boolean</td>
     <td>true if the request is malformed, false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>isAuthorized</td>
     <td>AuthResult</td>
     <td>if AuthSuccess is returned request is considered to be authorized and processing will continue. If AuthFailure is returend a "401 Unauthorized" response is returned with the value of the "WWW-Authenticate" header set to the string value given by the AuthFailure</td>
     <td>AuthSuccess</td>
   </tr>
   <tr>
     <td>isForbidden</td>
     <td>Boolean</td>
     <td>true if request is forbidden, false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>contentHeadersValid</td>
     <td>Boolean</td>
     <td>true if Content-* headers are valid, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>isKnownContentType</td>
     <td>Boolean</td>
     <td>true if content type is known, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>isValidEntityLength</td>
     <td>Boolean</td>
     <td>true if request body length is valid, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>options</td>
     <td>Map[HTTPHeader,String]</td>
     <td>additional headers to include in a response to an OPTIONS request</td>     
     <td>no additional headers</td>
   </tr>
   <tr>
     <td>contentTypesProvided</td>
     <td>Resource.ContentTypesProvided</td>
     <td>Determines the body of GET/HEAD requests. If a resource response to them, make sure to implement this function because the default will most likely not be suitable. It should return a list of 2-tuples, containing the content type and its associated body rendering function. The body rendering functions have the signature "ReqRespData => (Res[Array[Byte]], ReqRespData)". This return type is aliased by Resource.ContentTypesProvided</td>
     <td>text/html content type is provided with a default HTML body as a result</td>
   </tr>
   <tr>
     <td>isLanguageAvailable</td>
     <td>Boolean</td>
     <td>true if the Accept-Language is available, false otheriwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>charsetsProvided</td>
     <td>Resource.CharsetsProvided</td>
     <td>Determines the charset of the response body and influences whether or not a request can be serviced by this resource based on the "Accept-Charset". If the charset is acceptable the charsetting function will be run. The charsetting function type is Array[Byte] => Array[Byte]. A resource can choose to short-circuit charset negotiation by returning None instead of a Some value containing the list of acceptable charsets. This is encoded by the type alias Resource.CharsetsProvided</td>
     <td>None -- charset short-circuiting</td>
   </tr>
   <tr>
     <td>encodingsProvided</td>
     <td>Resource.EncodingsProvided</td>
     <td>Determines the encoding of the response body and influences whether or not a request can be serviced by this resource based on the "Accept-Encoding". If the encoding is acceptable the encoder function will be run. The encoder function type is Array[Byte] => Array[Byte]. A resource can choose to short-circuit encoding negotiation by returning None instead of a Some value containing the list of acceptable encodings. This is encoded by the type alias Resource.EncodingsProvided</td>
     <td>Some(("identity", identity[Array[Byte]](_)))</td>
   </tr>
   <tr>
     <td>resourceExists</td>
     <td>Boolean</td>
     <td>true if the resource exists, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>variances</td>
     <td>Seq[String]</td>
     <td>headers to be included in the "Vary" response header. "Accept", "Accept-Encoding" and "Accept-Charset" will always be included and do not need to be in the returned list</td>
     <td>Nil</td>
   </tr>
   <tr>
     <td>generateEtag</td>
     <td>Option[String]</td>
     <td>the etag, if one exists, for the resource to be included in the response and used when determining results to conditonal requests</td>
     <td>None</td>
   </tr>
   <tr>
     <td>lastModified</td>
     <td>Option[Date]</td>
     <td>the last modified date of the resource being request, if any, included in the response and used when determining requests to conditional requests </td>
     <td>None</td>
   </tr>
   <tr>
     <td>movedPermanently</td>
     <td>Option[String]</td>
     <td>None if the resource has not been moved, Some containing the URI of its new location otherwise</td>
     <td>None</td>
   </tr>
   <tr>
     <td>previouslyExisted</td>
     <td>Boolean</td>
     <td>true if the resource existed before, although it does not now, false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>movedTemporarily</td>
     <td>Option[String]</td>
     <td>None if the resource has not been moved temporarily, Some containing its temporary URI otherwise</td>
     <td>None</td>
   </tr>
   <tr>
     <td>allowMissingPost</td>
     <td>Boolean</td>
     <td>true if the resource allows POST requests to be serviced when the resource does not exist, false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>deleteResource</td>
     <td>Boolean</td>
     <td>perform the deletion of the resource during a DELETE request. return true if the deletion has succeeded (although it does not necessarily have to have completed), false otherwise</td>
     <td>false</td>
   </tr>
   <tr>
     <td>deleteCompleted</td>
     <td>Boolean</td>
     <td>true if the deleted is complete at the time this function is called, false otherwise</td>
     <td>true</td>
   </tr>
   <tr>
     <td>postIsCreate</td>
     <td>Boolean</td>
     <td>Determined whether or not "createPath" or "processPost" is called during a POST request. If true is returned "createPath" will be called, if false, "processPost" will be called. false should be returned when handling of a POST is simple processing, true should be returned when the request will actually create a resource</td>
     <td>false</td>
   </tr>
   <tr>
     <td>processPost</td>
     <td>Boolean</td>
     <td>true if the processing of the request succeed, false otherwise</td>
     <td>false</td>     
   </tr>
   <tr>
     <td>createPath</td>
     <td>Option[String]</td>
     <td>Used for POST request that represent creation of data. If this function is called it must return `Some` containing the created path. Returning None will result in a "500 Interal Error". The created path will be set as the dispatch path in the request data</td>
     <td>None</td>     
   </tr>
   <tr>
     <td>contentTypesAccepted</td>
     <td>Resource.ContentTypesAccepted</td>
     <td>Used during PUT and POST (if "postIsCreate" returns true). Returns a list of accepted content types and their associated handler functions. The halder functions have type ReqResData => (Res[Boolean],ReqRespData). If the handler returns true, processing is considered successful, otherwise it is considered to have failed and to be an error. If the content type of the request is not acceptable a response with code 415 is returned.</td>
     <td>Nil</td>     
   </tr>
   <tr>
     <td>isConflict</td>
     <td>Boolean</td>
     <td>true if a PUT request cannot be handled due to conflict, false otherwise</td>
     <td>false</td>     
   </tr>
   <tr>
     <td>expires</td>
     <td>Option[Date]</td>
     <td>if Some the date will be set as the value of the Expires header in the response</td>
     <td>None</td>     
   </tr>
   <tr>
     <td>multipleChoices</td>
     <td>Boolean</td>
     <td>if true, a "300 Mutliple Choices" response is returned to the client, otherwise a "200 OK" is returned</td>     
     <td>false</td>     
   </tr>
 </tbody>
</table>



