package no.penger.crud

import scala.util.Try

/**
 * The purpose of NamedCell is to bring the cells to a more crude level of abstraction, where
 *  we leave behind the types of cells, and instead match them with db columns by the column name.
 *  In this sense these are the binding between string values from the web frontend and the database.
 *
 *  Things obviously still work, because we have already propagated cells (through Editable.cells())
 *   so validation and rendering is done properly.
 */
trait namedCells extends cells {
  trait NamedCell {
    def tryCast(s:String): Try[Any]
    def fixed(value:Any): xml.NodeSeq
    def editable(value:Any): xml.NodeSeq
    def link(base:String, value:Any): xml.NodeSeq
    def name:String
  }

  def NamedCell(nme: String, cell: Cell[_]):NamedCell = new NamedCell {
    def tryCast(s: String): Try[Any]   = cell.tryCast(s)
    def fixed(value: Any)              = cell.asInstanceOf[Cell[Any]].fixed(value)
    def editable(value: Any)           = cell.asInstanceOf[Cell[Any]].editable(value)
    def link(base: String, value: Any) = cell.asInstanceOf[Cell[Any]].link(base, value)
    def name                           = nme
  }

}
