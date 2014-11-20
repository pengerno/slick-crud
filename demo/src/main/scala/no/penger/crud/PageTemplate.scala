package no.penger.crud

object PageTemplate {
  def page(ctx: String, title: String)(body: xml.NodeSeq) =
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title>{title}</title>
        <link rel="stylesheet" href={ctx + "/slick-crud/crud.css"}></link>
        <script                src={ ctx + "/slick-crud/crud.js"} type="text/javascript"></script>
      </head>
      <body>
        <div>{body}</div>
      </body>
    </html>
}