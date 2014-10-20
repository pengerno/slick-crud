package no.penger.crud

import scala.slick.lifted.Query

trait namedCells extends cells with queryParser {

  type NamedUntypedCell = (ColumnName, Cell[Any])

  object NamedCells {
    def apply[E, U](q: Query[E, U, Seq])(implicit e: CellRow[U]): NamedCells[U] =
      NamedCells(QueryParser columnNames q zip e.cells map {
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      })
  }

  case class NamedCells[T](cells: Seq[NamedUntypedCell])(implicit val cellRow: CellRow[T]){

    def cellByName(Name: ColumnName): Option[Cell[Any]] =
      cells collectFirst { case (Name, c) => c }

    def cellsWithUnpackedValues(row: T): Seq[(NamedUntypedCell, Any)] =
      cells zip (cellRow unpackValues row)

    def colNames = cells map (_._1)

    def extractCell[C](r: T, colName: ColumnName, col: Cell[C]): C = {
      val idIdx = colNames.indexOf(colName)
      cellRow.unpackValues(r)(idIdx).asInstanceOf[C]
    }

    def parseRow(params: Map[ColumnName, String]): Either[Seq[Error], T] =
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
