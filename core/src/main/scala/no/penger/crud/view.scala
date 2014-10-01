package no.penger.crud

import scala.xml.NodeSeq

trait view extends viewFormat {

  def append(one: ViewFormat, two: ViewFormat): ViewFormat

  def EditorView(ctx: String, uniqueId: String, tableName: TableName): EditorView

  trait EditorView{
    type RenderedNamedValue = (TableColumn, NodeSeq)
    def uniqueId: String
    def tableName: TableName

    def many(rows: Seq[Seq[ViewFormat]], cs: Seq[TableColumn]): ViewFormat
    def rowOpt(id: Option[String], rowOpt: Option[Seq[ViewFormat]], cs: Seq[TableColumn]): ViewFormat
  }
}