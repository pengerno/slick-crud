package no.penger.crud

import scala.slick.lifted.Query
import scala.util.Try

trait namedCells extends cells with queryParser {

  type NamedUntypedCell = (ColumnName, Cell[Any])

  object NamedCells {
    def apply[E, U](q: Query[E, U, Seq])(implicit e: CellRow[U]): NamedCells[U] =
      NamedCells(QueryParser columnNames q zip e.cells map {
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      })
  }

  case class NamedCells[T: CellRow](cells: Seq[NamedUntypedCell]){
    private def cellRow = implicitly[CellRow[T]]

    def cellByName(name: ColumnName): Try[Cell[Any]] =
      cells find (_._1 =:= name) map (_._2) toTry s"table does not have a column $name"

    def cellsWithUnpackedValues(row: T): Seq[(NamedUntypedCell, Any)] =
      cells zip cellRow.unpackValues(row)

    def colNames = cells map (_._1)

    def extractColumn[C](r: T, colName: ColumnName, col: Cell[C]): C = {
      val idIdx = colNames.indexOf(colName)
      cellRow.unpackValues(r)(idIdx).asInstanceOf[C]
    }
    def packValues(as: Seq[Any]) = cellRow packValues as
  }
}
