package no.penger
package crud

import java.util.UUID
import scala.slick.lifted.AbstractTable
import scala.reflect.ClassTag

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
     * @param pk a function that maps a projection L to its primary key column. Multi-column primary keys are not supported.

     * @tparam ROW row type for the database 'table'
     * @tparam LP the lifted projection, for example (Column[Int], Column[String])
     * @tparam P the projection, for example (Int, String)
     * @tparam ID the primary key column, for example Column[Int]
     *
     */
    def apply[ROW <: AbstractTable[_], LP : ClassTag, P : CellRow, ID: Cell : BaseColumnType]
      (mounted:    String,
       table:      TableQuery[ROW],
       notifier:   UpdateNotifier     = new UpdateNotifier,
       isEditable: Boolean            = true)
      (query:      Query[ROW, ROW#TableElementType, Seq] => Query[LP, P, Seq],
       pk:         ROW => Column[ID]) =
        new Editor[ROW, LP, P, ID](mounted, table, query, pk, notifier, isEditable, editors = Nil, isOnlyOneRow = false)
  }

  case class Editor[ROW <: AbstractTable[_], LP : ClassTag, P: CellRow, ID: Cell : BaseColumnType] (
      mounted:      String,
      table:        Query[ROW, ROW#TableElementType, Seq],
      query:        Query[ROW, ROW#TableElementType, Seq] => Query[LP, P, Seq],
      pk:           ROW => Column[ID],
      notifier:     UpdateNotifier,
      isEditable:   Boolean,
      editors:      Seq[ID => Editor[_, _, _, _]],
      isOnlyOneRow: Boolean) extends EditorAbstract[ID] {

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:ROW => Column[X])(x:X) = copy(table = table.filter(f(_) === x))

    /* return a new editor with referenced subeditors */
    def sub(editors:(ID => Editor[_, _, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(isOnlyOneRow = true)

    def base(ctx: String) = ctx + mounted

    val primaryKeys = QueryParser.primaryKeys(table.map(pk))
    val tableName   = QueryParser.tableNameFrom(table)

    val idCell      = implicitly[Cell[ID]]

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    val uniqueId    = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    def view(ctx: String) = {
      val rows        = db.withSession(
        implicit s => crudAction.read(base(ctx), primaryKeys, query(table), isEditable, max = Some(1).filter(_ => isOnlyOneRow))
      )
      val columnNames = QueryParser.columnNames(query(table))

      val view        = View(base(ctx), uniqueId, tableName, columnNames)

      if (isOnlyOneRow) view.rowOpt(None, rows.headOption)
      else              view.many(rows)
    }

    def viewRow(ctx: String, id:ID) = {
      val selectQuery = query(table.filter(pk(_) === id))
      val rowOpt      = db.withSession(
        implicit s => crudAction.read(base(ctx), primaryKeys, selectQuery, isEditable, max = Some(1)).headOption
      )
      val columnNames = QueryParser.columnNames(selectQuery)

      val view = View(base(ctx), uniqueId, tableName, columnNames).rowOpt(Some(idCell.fixed(id)), rowOpt)

      editors.map(_(id).view(ctx)).foldLeft(view)(append)
    }

    def update(id: ID, updates: Map[ColumnName, String]): Either[Seq[FailedUpdate], Seq[Update]] = {
      val filteredTable = table.filter(pk(_) === id)
      db.withTransaction{ implicit s =>
        crudAction.update(query(filteredTable), updates) match {
          case l@Left(fails: Seq[FailedUpdate]) =>
            s.rollback()
            fails foreach notifier.updateFailed(tableName, id)
            l
          case r@Right(okUpdates) =>
            okUpdates foreach notifier.updated(tableName, id)
            r
        }
      }
    }
  }
}