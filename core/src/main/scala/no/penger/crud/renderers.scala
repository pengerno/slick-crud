package no.penger.crud

trait renderers extends tableRefs with renderFormat {

  def combine(one: PageFormat, two: PageFormat): PageFormat

  def Renderer[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]): Renderer[ID, P]

  abstract class Renderer[ID: Cell, P] {
    def rows[T](rows: Seq[(ID, P)], via: Option[(ColumnName, T)]): PageFormat
    def row[T](id: ID, row: P, via: Option[(ColumnName, T)]): PageFormat
    def createRow[T](via: Option[(ColumnName, T)]): PageFormat
    def noRow[T](via: Option[(ColumnName, T)]): PageFormat
  }
}