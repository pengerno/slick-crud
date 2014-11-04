package no.penger
package crud

import scala.reflect.ClassTag
import scala.slick.lifted.AbstractTable

/**
 * An editor is the glue, and about the only thing clients really need to see
 *
 *  - render responses through a 'View'
 *  - talks to the database through 'crudActions'
 *  - renders and parses values through 'Cell's
 *  - sends notifications of (failed) updates thorough an 'UpdateNotifier'
 *  - composes with other 'Editor's (see 'on()', 'single()' and 'sub()')
 */
trait editors extends editorAbstracts with crudActions with view {
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
       notifier:   UpdateNotifier,
       isEditable: Boolean            = true)
      (query:      Query[ROW, ROW#TableElementType, Seq] ⇒ Query[LP, P, Seq],
       idColumn:   ROW ⇒ Column[ID])
      (implicit r: CellRow[ROW#TableElementType]) =
      new Editor[ROW, LP, P, ID](mounted, table, query, idColumn, notifier, isEditable, AstParser.tableName(table), editors = Nil, isOnlyOneRow = false)
  }

  case class Editor[ROW <: AbstractTable[_], LP: ClassTag, P: CellRow, ID: BaseColumnType] private (
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

    val primaryKey:      ColumnName                         = AstParser.colNames(table.map(idColumn)).head
    val namedCellsQuery: NamedCellRow[P]                    = NamedCellRow(query(table))
    val namedCellsTable: NamedCellRow[ROW#TableElementType] = NamedCellRow(table)

    def createView[T](ncr: NamedCellRow[T]) = View[ID, T](mounted, tableName, isEditable, primaryKey, ncr)

    override def view = {
      val view = createView(namedCellsQuery)

      if (isOnlyOneRow){
        crudAction.readRow(query(table)) match {
          case Some(row)       ⇒ view.single(namedCellsQuery.extractCell(row, primaryKey, idCell), row)
          case _               ⇒ view.notFound(None)
        }
      } else view.many {
        crudAction.read(query(table)).map {
          row ⇒ (namedCellsQuery.extractCell(row, primaryKey, idCell), row)
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

    override def viewNew = createView(namedCellsTable).newPage

    override def update(id: ID, columnName: ColumnName, value: String) =
      crudAction.update(namedCellsQuery, table, idColumn, id, namedCellsTable, columnName, value) mapBoth (
        error    ⇒ res.UpdateFailed(tableName, id, error, columnName, value),
        oldValue ⇒ res.Updated(tableName, id, columnName, oldValue, value)
      ) sideEffects (notifier.notifyUpdateFailure, notifier.notifyUpdated)

    override def create(params: Map[ColumnName, String]) =
      crudAction.create(table, namedCellsTable, idColumn, params, primaryKey) mapBoth (
        errors ⇒ res.CreateFailed(tableName, errors),
        id     ⇒ res.Created(tableName, id)
      ) sideEffects (notifier.notifyUpdateFailure, notifier.notifyUpdated)

    override def delete(id: ID) =
      crudAction.delete(table, idColumn, id) mapBoth (
        error ⇒ res.DeleteFailed(tableName, id, error),
        _     ⇒ res.Deleted(tableName, id)
      ) sideEffects (notifier.notifyUpdateFailure, notifier.notifyUpdated)
  }
}