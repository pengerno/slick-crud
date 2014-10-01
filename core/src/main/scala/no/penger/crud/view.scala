package no.penger.crud

import unfiltered.response.ResponseFunction

import scala.xml.NodeSeq

trait view[F] {

  def respond(base: String, title: String)(body: F): ResponseFunction[Any]

  def newView(ctx: String, uniqueId: String, tableName: TableName): EditorView

  trait EditorView{
    type RenderedNamedValue = (TableColumn, NodeSeq)
    def uniqueId: String
    def tableName: TableName

    def many(rows: Seq[Seq[NodeSeq]], cs: Seq[TableColumn]): F
    def rowOpt(id: Option[String], rowOpt: Option[Seq[NodeSeq]], cs: Seq[TableColumn]): F
  }
}