package no.penger.crud

import scala.slick.lifted.Query

trait namedCellRows extends cells with astParser {

  type NamedUntypedCell = (ColumnName, Cell[Any])

  object NamedCellRow {
    def apply[LROW, ROW](q: Query[LROW, ROW, Seq])(implicit e: CellRow[ROW]): NamedCellRow[ROW] =
      NamedCellRow(AstParser colNames q zip e.cells map {
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      })
  }

  case class NamedCellRow[ROW](cells: Seq[NamedUntypedCell])(implicit val cellRow: CellRow[ROW]){

    def cellByName(Name: ColumnName): Option[Cell[Any]] =
      cells collectFirst { case (Name, c) => c }

    def cellsWithUnpackedValues(row: ROW): Seq[(NamedUntypedCell, Any)] =
      cells zip (cellRow unpackValues row)

    def colNames = cells map (_._1)

    def extractCell[C](row: ROW, colName: ColumnName, col: Cell[C]): C = {
      val idIdx = colNames.indexOf(colName)
      cellRow.unpackValues(row)(idIdx).asInstanceOf[C]
    }

    def parseRow(params: Map[ColumnName, String]): Either[Seq[Error], ROW] =
      sequence {
        cells map {
          case (columnName, cell) ⇒
            params get columnName match {
              case Some(paramValue) ⇒ cell fromStr paramValue
              case _                ⇒ Left(ErrorMsg(s"Didn't provide value for $columnName"))
            }
        }
      }.right.map(cellRow.packValues)
  }
}
