package scalamachine.core
package dispatch

import org.slf4j.LoggerFactory

trait DispatchLogging extends Dispatch {

  private val logger = LoggerFactory.getLogger(classOf[Dispatch])

  abstract override def perform(route: Route, resource: Resource, data: ReqRespData): ReqRespData = {
    logger.info("performing route %s with resource %s, host info = %s, path info = %s".format(route, resource.getClass.getCanonicalName, data.hostInfo, data.pathInfo))
    super.perform(route, resource, data)
  }

}