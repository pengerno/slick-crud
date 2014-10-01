package no.penger.crud

import javax.servlet.http.HttpServletRequest

import unfiltered.request.HttpRequest
import unfiltered.response.ResponseFunction

import scala.xml.{Elem, NodeSeq}

trait view {
  type RenderedNamedValue = (String, NodeSeq)

  def presentPage[T <: HttpServletRequest](req: HttpRequest[T], title: String)(body: NodeSeq): ResponseFunction[Any]

  object WebIntegration {
    def single(editorPath: String, tableId: String, tablename: String, rowCells: Seq[RenderedNamedValue]): Elem =
      <div>
        <h2>{tablename}</h2>
        <script type="text/javascript">{s"no.penger.crud.single('$editorPath', '#$tableId')"}</script>
        <table id={tableId}>
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {rowCells.map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
        </table>
      </div>

    def many(editorPath: String, tableId: String, tablename: String, rows: Seq[Seq[NodeSeq]], columnNames: Seq[String]): Elem =
      <div>
        <h2>{tablename}</h2>
        <script type="text/javascript">no.penger.crud.view('{editorPath}', '#{tableId}')</script>
        <table id={tableId}>
          <thead><tr>{columnNames.map(name => <th>{name}</th>)}</tr></thead>
          {rows.map{ row => <tr>{row}</tr>}}
        </table>
      </div>

    def view404(tablename: String, idOpt: Option[String]): Elem = idOpt match {
      case Some(id) => <h1>{s"Could not find a $tablename for $id"}</h1>
      case None     => <h1>{s"Could not find this $tablename"}</h1>
    }
  }
}
