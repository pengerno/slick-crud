package no.penger.crud
package html

import scala.xml.{Elem, NodeSeq}

trait viewHtml extends view with viewFormatHtml {

  override def append(one: NodeSeq, two: NodeSeq) =
    one ++ two

  override def View(base: String, uniqueId: String, tableName: TableName, columnNames: Seq[TableColumn]) =
    ViewHtml(base, uniqueId, tableName, columnNames)

  case class ViewHtml(base: String, uniqueId: String, tableName: TableName, columnNames: Seq[TableColumn]) extends View {

    override def many(rows: Seq[Seq[Elem]]) =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        <table id={uniqueId}>
          <thead><tr>{columnNames.map(name => <th>{name}</th>)}</tr></thead>
          {rows.map{ row => <tr>{row}</tr>}}
        </table>
      </div>

    override def rowOpt(id: Option[Elem], rowOpt: Option[Seq[Elem]]) =
      rowOpt match {
        case None      => view404(id)
        case Some(row) => single(columnNames zip row)
      }

    def single(rowCells: Seq[(TableColumn, NodeSeq)]) =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
        <table id={uniqueId}>
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {rowCells.map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
        </table>
      </div>

    def view404(idOpt: Option[Elem]) = idOpt match {
      case Some(id) => <h1>{s"Could not find a $tableName for $id"}</h1>
      case None     => <h1>{s"Could not find this $tableName"}</h1>
    }
  }
}
