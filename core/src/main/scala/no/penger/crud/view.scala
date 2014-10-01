package no.penger.crud

import scala.xml.NodeSeq

trait view {
  /* the type of whatever we render to, for example NodeSeq */
  type ViewFormat

  def append(one: ViewFormat, two: ViewFormat): ViewFormat

  def EditorView(ctx: String, uniqueId: String, tableName: TableName): EditorView

  trait EditorView{
    type RenderedNamedValue = (TableColumn, NodeSeq)
    def uniqueId: String
    def tableName: TableName

    def many(rows: Seq[Seq[NodeSeq]], cs: Seq[TableColumn]): ViewFormat
    def rowOpt(id: Option[String], rowOpt: Option[Seq[NodeSeq]], cs: Seq[TableColumn]): ViewFormat
  }
}