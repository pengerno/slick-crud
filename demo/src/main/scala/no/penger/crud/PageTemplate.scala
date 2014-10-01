package no.penger.crud

import javax.servlet.http.HttpServletRequest

import unfiltered.filter.request.ContextPath
import unfiltered.request.HttpRequest

case class PageTemplate[T <: HttpServletRequest](req: HttpRequest[T]) {

  val ctx = req match {
    case ContextPath(c, _) => c
  }

  def javascript(ctx: String, path: String) =
    <script src={ctx + path} type="text/javascript"> </script>

  def css(ctx: String, path: String) =
    <link href={ctx + path} rel="stylesheet"/>
  
  def page(title: String)(body: xml.NodeSeq) =
    <html>
      <head>
        <title>{title}</title>
        { javascript(ctx, "/scripts/crud.js")}
        { javascript("", "http://code.jquery.com/jquery-2.1.0.min.js")}
        { css(ctx, "/crud.css")}
      </head>
      <body>
        <div class="content">
          <h1>{title}</h1>
          {body}
        </div>
      </body>
    </html>
}