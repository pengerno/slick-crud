package no.penger.crud

object PageTemplate {

  def javascript(ctx: String, path: String) =
    <script src={ctx + path} type="text/javascript"> </script>

  def css(ctx: String, path: String) =
    <link href={ctx + path} rel="stylesheet"/>
  
  def page(ctx: String, title: String)(body: xml.NodeSeq) =
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