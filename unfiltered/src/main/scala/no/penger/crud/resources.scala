package no.penger.crud

import unfiltered.filter.Plan.Intent
import unfiltered.filter.request.ContextPath
import unfiltered.response._

trait resources {
  val whitelist = List(
    "/slick-crud/crud.js",
    "/slick-crud/crud.css",
    "/slick-crud/3rdparty/jquery-1.11.1.min.js"
  )

  /* this will serve slick-cruds frontend resources, you can override it if you want to do that yourself */
  def resourceIntent: Intent = {
    case ContextPath(_, resource) if whitelist.contains(resource) =>
      Option(getClass.getResourceAsStream(resource)).fold[ResponseFunction[Any]](NotFound) (
        is ⇒ ResponseString(io.Source.fromInputStream(is).mkString)
      )
  }
}
