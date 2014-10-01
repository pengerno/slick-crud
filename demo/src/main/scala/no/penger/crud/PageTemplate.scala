package no.penger.crud

import unfiltered.response.{HtmlContent, ResponseString}

import scala.xml.{Attribute, Elem, NodeSeq, Text}

object PageTemplate {

  def checked(elem: Elem, checked: Boolean) = if (checked) elem % Attribute("checked", Seq(Text("checked")), xml.Null) else elem
  def Html5(ns: NodeSeq)                    = HtmlContent ~> ResponseString("<!DOCTYPE html>\n" + ns.toString)
  def javascript(ctx: String, path: String) = <script src={ctx + path} type="text/javascript"> </script>
  def css(ctx: String, path: String)        = <link href={ctx + path} rel="stylesheet"/>
  
  def page(ctx: String, title: String, pageCss: Seq[String] = Nil, scripts: Seq[String] = Nil, headContent: Seq[Elem] = Nil)(body: NodeSeq) = Html5 {
    <html>
      <head>
        <title>{title}</title>
        { javascript(ctx, "/scripts/crud.js")}
        { javascript("", "http://code.jquery.com/jquery-2.1.0.min.js")}
        { css(ctx, "/crud.css")}
        { headContent.map(e => e) }
        { scripts.flatMap(script => javascript(ctx, script)) }
        { pageCss.flatMap(c => css(ctx, c)) }
      </head>
      <body>
        <div class="menu">
        </div>
        <div class="content">
          <h1>
            { title }
          </h1>
          { body }
        </div>
      </body>
    </html>
  }
}