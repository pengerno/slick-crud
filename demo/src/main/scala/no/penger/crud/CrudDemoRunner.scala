package no.penger.crud

import no.penger.db.LiquibaseH2TransactionComponent

import com.typesafe.scalalogging.slf4j.LazyLogging
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.resource.ResourceCollection
import org.eclipse.jetty.webapp.WebAppContext
import unfiltered.jetty.Http

object CrudDemoRunner
  extends App
  with LiquibaseH2TransactionComponent
  with LazyLogging {

  val applications = List[Handler](
    webapp("/", "demo"),
    h2("/h2")
  )

  val server = Http(8080)

  for(app <- applications){
    server.handlers.addHandler(app)
  }

  server.run()

  def webapp(contextPath:String, location:String) = {
    val web = new WebAppContext()
    web.setContextPath(contextPath)
    web.setInitParameter("org.eclipse.jetty.servlet.Default.aliases", "true")
    web.setBaseResource(new ResourceCollection(Array(
      location + "/src/main/webapp",
      location + "/src/main/resources"
    )))
    web
  }

  def h2(contextPath:String) = {
    val web = new ServletContextHandler(ServletContextHandler.SESSIONS)
    web.setInitParameter("org.eclipse.jetty.servlet.Default.aliases", "true")
    web.setContextPath(contextPath)
    web.addServlet(classOf[org.h2.server.web.WebServlet], "/*")
    web
  }
}
