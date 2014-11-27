package no.penger.crud

trait renderers extends tableRefs with renderFormat {

  def combine(one: PageFormat, two: PageFormat): PageFormat

  def Renderer[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]): Renderer[ID, P]

  abstract class Renderer[ID: Cell, P] {
    def cell(columnName: ColumnName, value: Any, cell: Cell[Any]): ElemFormat
    def rows(rows: Seq[(ID, P)]): PageFormat
    def row(id: ID, row: P): PageFormat
    def missingRow[T](knownColumn: Option[(ColumnName, T)]): PageFormat
  }
}