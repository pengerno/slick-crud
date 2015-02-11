package no.penger.crud

trait renderers extends tableRefs with renderFormat {

  def combine(one: PageFormat, two: PageFormat): PageFormat

  def Renderer[ID, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]): Renderer[ID, P]

  abstract class Renderer[ID, P] {
    def rows[T](mainTable: TableName, rows: Seq[(Option[ID], P)], via: Option[(ColumnInfo, T)]): PageFormat
    def row[T](mainTable: TableName, id: Option[ID], row: P, via: Option[(ColumnInfo, T)]): PageFormat
    def createRow[T](via: Option[(ColumnInfo, Option[T])]): PageFormat
    def noRow[T](via: Option[(ColumnInfo, Option[T])]): PageFormat
    def message(s: String): PageFormat
  }
}