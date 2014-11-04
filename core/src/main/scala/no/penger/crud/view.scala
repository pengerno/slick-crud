package no.penger.crud

trait view extends namedCellRows with viewFormat {

  def append(one: PageFormat, two: PageFormat): PageFormat

  def View[ID: Cell, ROW](
    base:       String,
    tableName:  TableName,
    isEditable: Boolean,
    id:         ColumnName,
    ncr:        NamedCellRow[ROW]): View[ID, ROW]

  abstract class View[ID: Cell, ROW] {
    def renderCell(columnName: ColumnName, value: Any, cell: Cell[Any]): ElemFormat
    def many(rows: Seq[(ID, ROW)]): PageFormat
    def single(id: ID, row: ROW): PageFormat
    def notFound(idOpt: Option[ID]): PageFormat
    def newPage: PageFormat
  }
}