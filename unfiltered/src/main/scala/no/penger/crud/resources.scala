package no.penger.crud

import unfiltered.filter.Plan.Intent
import unfiltered.filter.request.ContextPath
import unfiltered.response._

trait resources {
  val whitelist = List(
    "/3rdparty/bootstrap/3.2.0/css/bootstrap.min.css",
    "/3rdparty/bootstrap/3.2.0/css/bootstrap-theme.min.css",
    "/3rdparty/jquery-1.11.1.min.js",
    "/3rdparty/bootstrap/3.2.0/js/bootstrap.min.js",
    "/scripts/crud.js"
  )

  /* this will serve slick-cruds frontend resources, you can override it if you want to do that yourself */
  def resourceIntent: Intent = {
    case ContextPath(_, resource) if whitelist.contains(resource) =>
      Option(getClass.getResourceAsStream(resource)).fold[ResponseFunction[Any]](NotFound) (
        is ⇒ ResponseString(io.Source.fromInputStream(is).mkString)
      )
  }
}
