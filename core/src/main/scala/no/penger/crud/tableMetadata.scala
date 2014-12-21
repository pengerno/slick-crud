package no.penger.crud

import scala.slick.lifted.{Column, Query}

trait tableMetadata extends cells with astParser {
  object Metadata {

    def infer[ID, TABLE, P](q: Query[TABLE, P, Seq], idCol: TABLE ⇒ Column[ID])
                           (implicit cr: CellRow[P], icd: Cell[ID]): Metadata[ID, P] = {
      val idQuery = q.map(idCol)
      val IdName  = AstParser.colNames(idQuery).head
      val idCell  = PKCell(icd)

      /* inject idCell in cell list */
      val cells = cellsWithColumnNames(q) map {
        case (IdName,  _   ) ⇒ (IdName,  idCell.asInstanceOf[Cell[Any]])
        case (colName, cell) ⇒ (colName, cell.asInstanceOf[Cell[Any]])
      }

      Metadata(cr.unpackValues, cr.packValues, cells, idCell, IdName)
    }

    def derive[TABLE, P, OID, OP](q: Query[TABLE, P, Seq], origin: Metadata[OID, OP])
                                 (implicit cr: CellRow[P]): Metadata[OID, P] = {
      /* prefer to keep override cells from origin */
      val cells = cellsWithColumnNames(q) map {
        case (colName, cell) ⇒
          origin.cellByName(colName) match {
            case Some(existing) ⇒ (colName, existing)
            case _              ⇒ (colName, cell.asInstanceOf[Cell[Any]])
          }
      }

      Metadata(cr.unpackValues, cr.packValues, cells, origin.idCell, origin.idColName)
    }

    def withReferencingRow[ID, LP, P, C: Cell]
                          (q: Query[(Column[C], LP), (C, P), Seq],
                           origin: Metadata[ID, P]): Metadata[ID, (C, P)] = {

      def unpackValues(cp: (C, P)) = cp._1 +: origin.unpackValues(cp._2)
      def packValues(as: Seq[Any]): (C, P) = (as.head.asInstanceOf[C], origin.packValues(as.tail))
      val cells: Seq[(ColumnName, Cell[Any])] = (ColumnName("referenced"), implicitly[Cell[C]].asInstanceOf[Cell[Any]]) +: origin.cells

      Metadata[ID, (C, P)](unpackValues, packValues, cells, origin.idCell, origin.idColName)
    }

    private def cellsWithColumnNames[TABLE, P](q: Query[TABLE, P, Seq])(implicit cr: CellRow[P]): Seq[(ColumnName, Cell[_])] =
      AstParser colNames q zip cr.cells
  }

  case class Metadata[ID, P](unpackValues: P ⇒ List[Any],
                            packValues:   Seq[Any] ⇒ P,
                            cells:        Seq[(ColumnName, Cell[Any])],
                            idCell:       Cell[ID],
                            idColName:    ColumnName) {

    def cellByName(Name: ColumnName): Option[Cell[Any]] =
      cells collectFirst { case (Name, c) => c }

    def cellsWithUnpackedValues(row: P): Seq[((ColumnName, Cell[Any]), Any)] =
      cells zip unpackValues(row)

    def colNames = cells map (_._1)

    def extractIdFromRow(row: P): Option[ID] = {
      colNames.indexOf(idColName) match {
        case -1  ⇒ None
        case idx ⇒ Some(unpackValues(row)(idx).asInstanceOf[ID])
      }
    }

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

    def withFkCell[COL](colQ: Query[_, _, Seq], wrapper: Cell[COL] ⇒ Cell[COL]): Metadata[ID, P] = {
      val colName  = AstParser.colNames(colQ).head

      /* inject foreign key cell unless its a pk cell */
      val newCells = cells.map {
        case cell@(`colName`, PKCell(_)) ⇒ cell
        case      (`colName`, cell)      ⇒ (colName, wrapper(cell.asInstanceOf[Cell[COL]]).asInstanceOf[Cell[Any]])
        case cell                        ⇒ cell
      }

      copy(cells = newCells)
    }
  }
}
