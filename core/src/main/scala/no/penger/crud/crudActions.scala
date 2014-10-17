package no.penger.crud

import scala.slick.lifted.AbstractTable
import scala.util.Try

trait crudActions extends namedCells with columnPicker with databaseIntegration {
  import profile.simple._

  object crudAction {

    def read[E, U](q: Query[E, U, Seq]): List[U] =
      db withSession (implicit s ⇒ q.list)

    def readRow[E, U](q: Query[E, U, Seq]): Option[U] =
      db withSession (implicit s ⇒ q.firstOption)

    /**
     * Update a row in a table with the key -> values in 'updates'
     *
     * We take two sets of named cells here. 'namedCellsQuery' just to verify that the
     *  columns to be updated are exposed by the current query, and we use 'namedCellsTable'
     *  for the actual update
     */
    def update[TABLE <: AbstractTable[_]](
        namedCellsQuery: NamedCells[_],
        table:           Query[TABLE, TABLE#TableElementType, Seq],
        namedCellsTable: NamedCells[TABLE#TableElementType],
        updates:         Map[ColumnName, String]): Either[Seq[UpdateFailed], Seq[UpdateSuccess]] =

      db.withTransaction {
        implicit s ⇒
          val results: Iterable[Either[UpdateFailed, UpdateSuccess]] = updates.map {
            case (columnName, value) ⇒
              val tried: Try[UpdateSuccess] = for {
                _              ← namedCellsQuery cellByName columnName
                cell           ← namedCellsTable cellByName columnName
                updater        ← Try(table map ColumnWithName(columnName))
                validValue     ← cell tryFromStr value
                oldValue       ← Try(updater.first)
                numUpdates     ← Try(updater update validValue)
              } yield UpdateSuccess(columnName, oldValue, validValue, numUpdates)

              tried.toEither.left.map {
                case t ⇒ UpdateFailed(columnName, value, t)
              }
        }

        sequence(results).sideEffects(_ ⇒ s.rollback(), _ ⇒ ())
      }


    def create[TABLE <: AbstractTable[_], ID: BaseColumnType](
        table:        Query[TABLE, TABLE#TableElementType, Seq],
        namedCells:   NamedCells[TABLE#TableElementType],
        idColumn:     TABLE ⇒ Column[ID],
        params:       Map[ColumnName, String]): Either[Seq[Throwable], Either[TABLE#TableElementType, ID]] = {

      val validatedValues: Seq[Either[Throwable, Any]] = namedCells.cells map {
        case (columnName, cell) ⇒
          val tried: Try[Any] = for {
            value      ← params get columnName toTry s"didn't provide value for $columnName"
            validValue ← cell tryFromStr value
          } yield validValue

          tried.toEither
      }

      sequence(validatedValues).right.flatMap {
        validValues ⇒ {
          val inserter = table returning table.map(idColumn)
          val toInsert = namedCells packValues validValues
          val ret      = Try(db withTransaction (implicit s ⇒ inserter insert toInsert))
          ret match {
            case util.Success(oid) ⇒ Right(Right(oid))
            case util.Failure(t1)  ⇒
              /* try again without return of autoincrement value*/
              Try(db withTransaction (implicit s ⇒ table insert toInsert )) match {
                case util.Success(_)  ⇒ Right(Left(toInsert))
                case util.Failure(t2) ⇒ Left(Seq(t2, t1))
              }
          }
        }
      }
    }
    
    def delete[TABLE <: AbstractTable[_], ID: BaseColumnType: Cell](
        table:    Query[TABLE, TABLE#TableElementType, Seq],
        idColumn: TABLE ⇒ Column[ID],
        id:       ID): Either[DeleteFailed, DeleteSuccess.type] = {

      val row = table.filter(idColumn(_) === id)

      /* the default implicit, queryToDeleteInvoker, only converts Query[_ <: Table[_]]) to DeleteInvoker,
       *  so I inlined it here to make it work for Query[_ <: AbstractTable[_]] too*/
      val deleteInvoker = new profile.DeleteInvoker(profile.deleteCompiler.run(row.toNode).tree, ())

      db withTransaction {
        implicit s ⇒
          deleteInvoker.delete.run match {
            case 0 ⇒ Left(DeleteFailed(s"No rows matched"))
            case 1 ⇒ Right(DeleteSuccess)
            case _ ⇒ s.rollback(); Left (DeleteFailed(s"matched ${deleteInvoker.delete.run} rows"))
          }
      }
    }
  }
}