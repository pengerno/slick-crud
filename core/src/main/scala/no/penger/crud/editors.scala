package no.penger
package crud

import scala.reflect.ClassTag
import scala.slick.lifted.AbstractTable

/**
 * An editor is the glue, and about the only thing clients really need to see
 *
 *  - render responses through a 'View'
 *  - talks to the database through an 'Editable'
 *  - renders and parses values through 'Cell's
 *  - sends notifications of (failed) updates thorough an 'UpdateNotifier'
 *  - composes with other 'Editor's (see 'on()', 'single()' and 'sub()')
 */
trait editors extends editorAbstracts with crudActions with view with updateNotifier {
  import profile.simple._

  object Editor{

    /**
     * Default entry point that a client should use to expose a table
     *
     * This constructor with multiple parameter sets exists just to drive type inference
     *
     * @param table the TableQuery[E] of the table we want to expose
     * @param notifier override this if you want side effects after updates
     * @param isEditable set this to false if the whole table should be read-only
     * @param query a mapping from the tables default projection, pass in identity if you just want the whole table
     * @param idColumn a function that maps a projection L to its primary key column. Multi-column primary keys are not supported.

     * @tparam ROW row type for the database 'table'
     * @tparam LP the lifted projection, for example (Column[Int], Column[String])
     * @tparam P the projection, for example (Int, String)
     * @tparam ID the primary key column, for example Column[Int]
     *
     */
    def apply[ROW <: AbstractTable[_], LP: ClassTag, P: CellRow, ID: BaseColumnType: Cell]
      (mounted:    String,
       table:      TableQuery[ROW],
       notifier:   UpdateNotifier     = new UpdateNotifier,
       isEditable: Boolean            = true)
      (query:      Query[ROW, ROW#TableElementType, Seq] ⇒ Query[LP, P, Seq],
       idColumn:   ROW ⇒ Column[ID])
      (implicit r: CellRow[ROW#TableElementType]) =
      new Editor[ROW, LP, P, ID](mounted, table, query, idColumn, notifier, isEditable, QueryParser.tableNameFrom(table), editors = Nil, isOnlyOneRow = false)
  }

  case class Editor[ROW <: AbstractTable[_], LP: ClassTag, P: CellRow, ID: BaseColumnType] (
      mounted:      String,
      table:        Query[ROW, ROW#TableElementType, Seq],
      query:        Query[ROW, ROW#TableElementType, Seq] ⇒ Query[LP, P, Seq],
      idColumn:     ROW ⇒ Column[ID],
      notifier:     UpdateNotifier,
      isEditable:   Boolean,
      tableName:    TableName,
      editors:      Seq[ID ⇒ Editor[_, _, _, _]],
      isOnlyOneRow: Boolean)
     (implicit val idCell:            Cell[ID],
                   tableCellRow:      CellRow[ROW#TableElementType]) extends EditorAbstract[ID] {

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:ROW ⇒ Column[X])(x:X) = copy(table = table.filter(f(_) === x))

    /* return a new editor with referenced subeditors */
    def sub(editors:(ID ⇒ Editor[_, _, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(isOnlyOneRow = true)

    val primaryKey:      ColumnName                       = QueryParser.columnNames(table.map(idColumn)).head
    val namedCellsQuery: NamedCells[P]                    = NamedCells(query(table))
    val namedCellsTable: NamedCells[ROW#TableElementType] = NamedCells(table)

    def createView[T](ncs: NamedCells[T]) = View[ID, T](mounted, tableName, isEditable, primaryKey, ncs)

    override def view = {
      val view = createView(namedCellsQuery)

      if (isOnlyOneRow){
        crudAction.readRow(query(table)) match {
          case Some(row)       ⇒ view.single(namedCellsQuery.extractColumn(row, primaryKey, idCell), row)
          case _               ⇒ view.notFound(None)
        }
      } else view.many {
        crudAction.read(query(table)).map {
          row ⇒ (namedCellsQuery.extractColumn(row, primaryKey, idCell), row)
        }
      }
    }

    override def viewRow(id:ID) = {
      val view   = createView(namedCellsQuery)
      val rowOpt = crudAction.readRow(query(table.filter(idColumn(_) === id)))

      rowOpt match {
        case Some(row) ⇒ editors.map(_(id).view).foldLeft(view.single(id, row))(append)
        case _         ⇒ view.notFound(Some(id))
      }
  }

    override def viewNew: PageFormat = createView(namedCellsTable).newPage

    override def update(id: ID, updates: Map[ColumnName, String]): Either[Seq[UpdateFailed], Seq[UpdateSuccess]] =
      crudAction.update(namedCellsQuery, table.filter(idColumn(_) === id), namedCellsTable, updates).sideEffects(
        _ foreach notifier.updateFailed(tableName, id),
        _ foreach notifier.updated(tableName, id)
      )

    override def create(params: Map[ColumnName, String]): Either[Seq[Throwable], ID] =
      crudAction.create(table, namedCellsTable, idColumn, params).right.map{
        case Right(autogeneratedId) ⇒ autogeneratedId
        /* if there was no auto-generated id, dig out the id from what we inserted */
        case Left(insertedRow)      ⇒ namedCellsTable.extractColumn(insertedRow, primaryKey, idCell)
      }.sideEffects(
        l ⇒ (),
        id ⇒ notifier.create(tableName, id)
      )

    override def delete(id:ID): Either[DeleteFailed, DeleteSuccess.type] = {
      crudAction.delete(table, idColumn, id).sideEffects(
        notifier.deleteFailed(tableName, id),
        _ ⇒ notifier.deleted(tableName, id)
      )
    }
  }
}