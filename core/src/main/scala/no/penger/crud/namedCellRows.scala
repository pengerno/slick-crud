package no.penger.crud

import scala.slick.lifted.{Column, Query}

trait namedCellRows extends cells with astParser {
  object NamedCellRow {
    def apply[P](q: Query[_, P, Seq])(implicit cr: CellRow[P]): NamedCellRow[P] =
      NamedCellRow(cr.unpackValues, cr.packValues, AstParser colNames q zip cr.cells map {
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      })

    private[NamedCellRow] def withWrappedCell[COL](cells: Seq[(ColumnName, Cell[Any])],
                                                   Name: ColumnName,
                                                   wrapper: Cell[COL] ⇒ Cell[COL]) =
      cells.map {
        case (Name, cell) ⇒ (Name, wrapper(cell.asInstanceOf[Cell[COL]]).asInstanceOf[Cell[Any]])
        case cell      ⇒ cell
      }
  }

  case class NamedCellRow[P](unpackValues: P ⇒ List[Any],
                             packValues:   Seq[Any] ⇒ P,
                             cells:        Seq[(ColumnName, Cell[Any])]){

    def cellByName(Name: ColumnName): Option[Cell[Any]] =
      cells collectFirst { case (Name, c) => c }

    def cellsWithUnpackedValues(row: P): Seq[((ColumnName, Cell[Any]), Any)] =
      cells zip unpackValues(row)

    def colNames = cells map (_._1)

    def extractCell[C](row: P, colName: ColumnName, col: Cell[C]): C =
      unpackValues(row)(colNames.indexOf(colName)).asInstanceOf[C]

    def parseRow(params: Map[ColumnName, String]): Either[Seq[Error], P] =
      sequence {
        cells map {
          case (columnName, cell) ⇒
            params get columnName match {
              case Some(paramValue) ⇒ cell fromStr paramValue
              case _                ⇒ Left(errorMsg(s"Didn't provide value for $columnName"))
            }
        }
      }.right.map(packValues)

    def withOverriddenCells[PP](other: NamedCellRow[PP]): NamedCellRow[P] =
      copy(cells = cells.map{case (name, cell) ⇒ name → other.cellByName(name).getOrElse(cell)})

    def withPkCell[ID](q: Query[Column[ID], ID, Seq])(implicit c: Cell[ID]): NamedCellRow[P] = {
      val idName = AstParser.colNames(q).head
      copy(cells = NamedCellRow.withWrappedCell(cells, idName, PKCell.apply))
    }

    def withFkCell[COL](q: Query[Column[COL], COL, Seq], wrapper: Cell[COL] ⇒ FKCell[COL]): NamedCellRow[P] = {
      val colName = AstParser.colNames(q).head
      copy(cells  = NamedCellRow.withWrappedCell(cells, colName, wrapper))
    }
  }
}
