package no.penger.crud

object PageTemplate {
  def page(ctx: String, title: String)(body: xml.NodeSeq) =
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <title>{title}</title>
        <link rel="stylesheet" href={ctx + "/3rdparty/bootstrap/3.2.0/css/bootstrap.min.css"}></link>
        <link rel="stylesheet" href={ctx + "/3rdparty/bootstrap/3.2.0/css/bootstrap-theme.min.css"}></link>
        <script                src={ ctx + "/3rdparty/jquery-1.11.1.min.js"}></script>
        <script                src={ ctx + "/3rdparty/bootstrap/3.2.0/js/bootstrap.min.js"}></script>
        <script                src={ ctx + "/scripts/crud.js"} type="text/javascript"></script>
      </head>
      <body>
        <p><div class="container-fluid">{body}</div></p>
      </body>
    </html>
}