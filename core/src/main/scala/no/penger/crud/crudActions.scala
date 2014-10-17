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
        q:               Query[TABLE, TABLE#TableElementType, Seq],
        namedCellsTable: NamedCells[TABLE#TableElementType],
        updates:         Map[ColumnName, String]): Either[Seq[FailedUpdate], Seq[Update]] =

      db.withTransaction {
        implicit s ⇒
          val results: Iterable[Either[FailedUpdate, Update]] = updates.map {
            case (columnName, value) ⇒
              val tried: Try[Update] = for {
                _              ← namedCellsQuery cellByName columnName
                cell           ← namedCellsTable cellByName columnName
                updater        ← Try(q.map(table ⇒ ColumnWithName(table, columnName)))
                validValue     ← cell tryFromStr value
                oldValue       ← Try(updater.first)
                numUpdates     ← Try(updater update validValue)
              } yield Update(columnName, oldValue, validValue, numUpdates)

              tried.toEither.left.map {
                case t ⇒ FailedUpdate(columnName, value, t)
              }
        }
        sequence(results).sideEffects(_ ⇒ s.rollback(), _ ⇒ ())
      }


    def create[TABLE <: AbstractTable[_], ID: BaseColumnType](
        table:        Query[TABLE, TABLE#TableElementType, Seq],
        namedCells:   NamedCells[TABLE#TableElementType],
        id:           TABLE ⇒ Column[ID],
        params:       Map[ColumnName, String]): Either[Seq[Throwable], Option[ID]] = {

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
          val inserter = table returning table.map(r ⇒ id(r).?)
          val toInsert = namedCells packValues validValues
          val ret      = Try(db withTransaction (implicit s ⇒ inserter insert toInsert))
          ret match {
            case util.Success(oid) ⇒ Right(oid)
            case util.Failure(t1)  ⇒
              /* try again without return of autoincrement value*/
              Try(db withTransaction (implicit s ⇒ table insert toInsert )) match {
                case util.Success(_)  ⇒ Right(None)
                case util.Failure(t2) ⇒ Left(Seq(t2, t1))
              }
          }
        }
      }
    }
  }
}