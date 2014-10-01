package no.penger.crud
package html

import scala.xml.NodeSeq

trait viewHtml extends view with viewFormatHtml {

  override def append(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def EditorView(base: String, uniqueId: String, tableName: TableName) =
    EditorViewHtml(base, uniqueId, tableName)

  case class EditorViewHtml(base: String, uniqueId: String, tableName: TableName) extends EditorView {

    override def many(rows: Seq[Seq[NodeSeq]], cs: Seq[TableColumn]) =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        <table id={uniqueId}>
          <thead><tr>{cs.map(name => <th>{name}</th>)}</tr></thead>
          {rows.map{ row => <tr>{row}</tr>}}
        </table>
      </div>

    override def rowOpt(id: Option[String], rowOpt: Option[Seq[NodeSeq]], cs: Seq[TableColumn]) =
      rowOpt match {
        case None      => view404(id)
        case Some(row) => single(cs zip row)
      }

    def single(rowCells: Seq[RenderedNamedValue]) =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
        <table id={uniqueId}>
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {rowCells.map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
        </table>
      </div>

    def view404(idOpt: Option[String]) = idOpt match {
      case Some(id) => <h1>{s"Could not find a $tableName for $id"}</h1>
      case None     => <h1>{s"Could not find this $tableName"}</h1>
    }
  }
}
