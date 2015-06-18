package no.penger.crud

import linx.StaticLinx

object PageTemplate {
  def page(ctx: String, title: String, tables: Seq[(TableName, StaticLinx)])(body: xml.NodeSeq) =
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title>{title}</title>
        <link rel="stylesheet" href={ctx + "/slick-crud/crud.css"}></link>
        <script src={ctx + "/slick-crud/3rdparty/jquery-1.11.1.min.js"} type="text/javascript"></script>
        <script src={ctx + "/slick-crud/crud.js"} type="text/javascript"></script>
      </head>
      <body>
        <table><tr>
          <td style="text-align:center"><h3>All tables:</h3></td>
          {tables map {
            case (tableName, link) =>
              <td style="text-align:center"><a class="btn-style" href={link()}>{tableName.toString}</a></td>
          }
        }</tr></table>
        <br/>
        <div>{body}</div>
      </body>
    </html>
}