package no.penger.crud

import scala.slick.lifted.AbstractTable
import scala.util.{Failure, Success, Try}

trait crudActions extends namedCellRows with columnPicker with databaseIntegration {

  import profile.simple._

  object crudAction {

    def read[E, U](q: Query[E, U, Seq]): List[U] =
      db withSession (implicit s ⇒ q.list)

    def readRow[E, U](q: Query[E, U, Seq]): Option[U] =
      db withSession (implicit s ⇒ q.firstOption)

    /**
     * Update a column 'columnName' for row with id 'id' with value 'value'
     *
     * We take two sets of named cells here. 'namedCellsQuery' just to verify that the
     *  columns to be updated are exposed by the current query, and we use 'namedCellsTable'
     *  for the actual update
     *
     *  @return old value of cell on success, error otherwise
     */
    def update[TABLE <: AbstractTable[_], ID: BaseColumnType : Cell](
        namedCellsQuery: NamedCellRow[_],
        table:           Query[TABLE, TABLE#TableElementType, Seq],
        idColumn:        TABLE ⇒ Column[ID],
        id:              ID,
        namedCellsTable: NamedCellRow[TABLE#TableElementType],
        columnName:      ColumnName,
        value:           String): Either[Error, String] =

      for {
        _              ← namedCellsQuery cellByName columnName orError s"projection has no cell with name $columnName"
        cell           ← namedCellsTable cellByName columnName orError s"table has no cell with name $columnName"
        validValue     ← cell fromStr value
        row            = table.filter(idColumn(_) === id)
                         //cookies to whoever can get rid of the .get() - 'cols' doesn't make sense outside the scope of map()
        updater        = row map (cols => ColumnWithName(columnName)(cols).get)
        oldValue       ← Try(db withSession (implicit s ⇒ updater.first)).toEither.left.map[Error](ErrorExc)
        _              ← db withTransaction (implicit s ⇒ ensureOneRowChanged(Try(updater update validValue)))
      } yield cell.toStr(oldValue)


    def create[TABLE <: AbstractTable[_], ID: BaseColumnType: Cell](
        table:        Query[TABLE, TABLE#TableElementType, Seq],
        namedCells:   NamedCellRow[TABLE#TableElementType],
        idColumn:     TABLE ⇒ Column[ID],
        params:       Map[ColumnName, String],
        primaryKey:   ColumnName): Either[Seq[Error], ID] = {

      def doInsert(toInsert: TABLE#TableElementType): Either[Seq[Error], ID] =
      /* first try and insert and see if we can get an id back */
        Try[ID](db withTransaction (implicit s ⇒ table returning table.map(idColumn) insert toInsert)).orElse {
          /* try again without return of autoincrement value */
          Try(db withTransaction (implicit s ⇒ table.insert(toInsert))).map{
            /* since there was no auto-generated id, dig out the id from what we inserted */
            _ ⇒ namedCells.extractCell(toInsert, primaryKey, implicitly[Cell[ID]])
          }
        }.toEither.left.map(t ⇒ Seq(ErrorExc(t)))

      for {
        toInsert ← namedCells.parseRow(params)
        id       ← doInsert(toInsert)
      } yield id
    }

    def delete[TABLE <: AbstractTable[_], ID: BaseColumnType: Cell](
        table:    Query[TABLE, TABLE#TableElementType, Seq],
        idColumn: TABLE ⇒ Column[ID],
        id:       ID): Either[Error, Unit] = {

      val row = table.filter(idColumn(_) === id)

      /* the default implicit, queryToDeleteInvoker, only converts Query[_ <: Table[_]]) to DeleteInvoker,
       *  so I inlined it here to make it work for Query[_ <: AbstractTable[_]] too*/
      val deleteInvoker = new profile.DeleteInvoker(profile.deleteCompiler.run(row.toNode).tree, ())

      db withTransaction (implicit s ⇒ ensureOneRowChanged(Try(deleteInvoker.delete)))
  }

    private def ensureOneRowChanged(tn: Try[Int])(implicit s: Session): Either[Error, Unit] = tn match {
      case Success(1)   ⇒ Right(())
      case Success(0)   ⇒ s.rollback(); Left(ErrorMsg(s"No rows matched"))
      case Success(num) ⇒ s.rollback(); Left(ErrorMsg(s"Rolled back because matched $num rows"))
      case Failure(t)   ⇒ s.rollback(); Left(ErrorExc(t))
    }
  }
}