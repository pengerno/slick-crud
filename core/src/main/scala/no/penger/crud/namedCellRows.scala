package no.penger.crud

import scala.slick.lifted.Query

trait namedCellRows extends cells with astParser {

  type NamedUntypedCell = (ColumnName, Cell[Any])

  object NamedCellRow {
    def apply[P](q: Query[_, P, Seq])(implicit e: CellRow[P]): NamedCellRow[P] =
      NamedCellRow(implicitly[CellRow[P]], AstParser colNames q zip e.cells map {
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      })
  }

  case class NamedCellRow[P](cellRow: CellRow[P], cells: Seq[NamedUntypedCell]){

    def cellByName(Name: ColumnName): Option[Cell[Any]] =
      cells collectFirst { case (Name, c) => c }

    def cellsWithUnpackedValues(row: P): Seq[(NamedUntypedCell, Any)] =
      cells zip (cellRow unpackValues row)

    def colNames = cells map (_._1)

    def extractCell[C](row: P, colName: ColumnName, col: Cell[C]): C =
      cellRow.unpackValues(row)(colNames.indexOf(colName)).asInstanceOf[C]

    def parseRow(params: Map[ColumnName, String]): Either[Seq[Error], P] =
      sequence {
        cells map {
          case (columnName, cell) ⇒
            params get columnName match {
              case Some(paramValue) ⇒ cell fromStr paramValue
              case _                ⇒ Left(errorMsg(s"Didn't provide value for $columnName"))
            }
        }
      }.right.map(cellRow.packValues)
  }
}
