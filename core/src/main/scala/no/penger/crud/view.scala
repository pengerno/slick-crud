package no.penger.crud

import unfiltered.response.ResponseFunction

import scala.xml.NodeSeq

trait view[F] {

  def newEditor(ctx: String, uniqueId: String, tableName: String): EditorView

  def respond(base: String, title: String)(body: F): ResponseFunction[Any]

  trait EditorView{
    type RenderedNamedValue = (String, NodeSeq)
    def uniqueId: String
    def tableName: String

    def many(rows: Seq[Seq[NodeSeq]], columnNames: Seq[String]): F
    def rowOpt(id: Option[String], rowOpt: Option[Seq[NodeSeq]], columnNames: Seq[String]): F
  }
}

trait htmlView extends view[NodeSeq] {
  override def newEditor(base: String, uniqueId: String, tableName: String) = htmlEditorView(base, uniqueId, tableName)

  case class htmlEditorView(base: String, uniqueId: String, tableName: String) extends EditorView{

    override def many(rows: Seq[Seq[NodeSeq]], columnNames: Seq[String]): NodeSeq =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">no.penger.crud.view('{base}', '#{uniqueId}')</script>
        <table id={uniqueId}>
          <thead><tr>{columnNames.map(name => <th>{name}</th>)}</tr></thead>
          {rows.map{ row => <tr>{row}</tr>}}
        </table>
      </div>

    override def rowOpt(id: Option[String], rowOpt: Option[Seq[NodeSeq]], columnNames: Seq[String]): NodeSeq =
      rowOpt match {
        case None      => view404(id)
        case Some(row) => single(columnNames zip row)
      }

    def single(rowCells: Seq[RenderedNamedValue]): NodeSeq =
      <div>
        <h2>{tableName}</h2>
        <script type="text/javascript">{s"no.penger.crud.single('$base', '#$uniqueId')"}</script>
        <table id={uniqueId}>
          <thead><tr><th>Column</th><th>Value</th></tr></thead>
          {rowCells.map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
        </table>
      </div>
    
    def view404(idOpt: Option[String]): NodeSeq = idOpt match {
      case Some(id) => <h1>{s"Could not find a $tableName for $id"}</h1>
      case None     => <h1>{s"Could not find this $tableName"}</h1>
    }
  }
}