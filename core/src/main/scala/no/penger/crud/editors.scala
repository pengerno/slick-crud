package no.penger
package crud

import java.util.UUID

import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.{&, GET, POST, Params, Seg}
import unfiltered.response.{ResponseFunction, BadRequest, Ok, ResponseString}

import scala.reflect.ClassTag

/**
 * An editor is the glue, and the only thing clients really need to see
 *
 *  - communicates through unfiltered (see intent)
 *  - render responses through a 'View'
 *  - talks to the database through an 'Editable'
 *  - renders and parses values through 'Cell's
 *  - sends notifications of (failed) updates thorough an 'UpdateNotifier'
 *  - composes with other 'Editor's (see 'on()', 'single()' and 'sub()')
 */
trait editors extends crudActions with view with updateNotifier {

  def respond(ctx: String, title: String)(body: ViewFormat): ResponseFunction[Any]

  import profile.simple._
  import scala.slick.lifted.AbstractTable

  object Editor {
    /**
     * Default entry point that a client should use to expose a table
     * 
     * This constructor with multiple parameter sets exists just to drive type inference
     * 
     * @param mount the url through which the exposed table can be reached
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
      (mount:      String,
       table:      TableQuery[ROW],
       notifier:   UpdateNotifier     = new UpdateNotifier,
       isEditable: Boolean            = true)
      (query:      Query[ROW, ROW#TableElementType, Seq] => Query[LP, P, Seq],
       pk:         ROW => Column[ID]) =

      new Editor[ROW, LP, P, ID](mount, table, query, pk, notifier, isEditable, editors = Nil, isOnlyOneRow = false)
  }

  case class Editor[ROW <: AbstractTable[_], LP : ClassTag, P: CellRow, ID: Cell : BaseColumnType] private (
      mount:        String,
      table:        Query[ROW, ROW#TableElementType, Seq],
      query:        Query[ROW, ROW#TableElementType, Seq] => Query[LP, P, Seq],
      pk:           ROW => Column[ID],
      notifier:     UpdateNotifier,
      isEditable:   Boolean,
      editors:      Seq[ID => Editor[_, _, _, _]],
      isOnlyOneRow: Boolean) {

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:ROW => Column[X])(x:X) = copy(table = table.filter(f(_) === x))

    /* return a new editor with referenced subeditors */
    def sub(editors:(ID => Editor[_, _, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(isOnlyOneRow = true)

    val primaryKeys = QueryParser.primaryKeys(table.map(pk))
    val tableName   = QueryParser.tableNameFrom(table)

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    val uniqueId    = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    val MountedAt   = Seg.unapply(mount).get
    def base(ctx: String) = (ctx :: MountedAt).mkString("/")

    /* extract id from url */
    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(MountedAt.size) match {
          case (MountedAt, id :: Nil) => implicitly[Cell[ID]].tryCast(id).toOption
          case _ => None
        }
    }

    def intent:Plan.Intent = {

      case req@GET(ContextPath(ctx, Seg(MountedAt))) =>
        respond(ctx, title = MountedAt.head)(view(ctx))

      case req@GET(ContextPath(ctx, Seg(Id(id)))) =>
        respond(ctx, title = s"$tableName for $id") (
          editors.map(_(id).view(ctx)).foldLeft(viewRow(ctx, id))(append)
        )

      case req@POST(ContextPath(ctx, Seg(Id(id)))) & Params(params) =>
        val updates = params.map {
          case (name, values) => ColumnName(name) -> values.head
        }
        update(ctx, id, updates)
    }

    def view(ctx: String): ViewFormat = {
      val rows        = db.withSession(implicit s => databaseAction.read(base(ctx), primaryKeys, query(table), isEditable, max = Some(1).filter(_ => isOnlyOneRow)))
      val columnNames = QueryParser.columnNames(query(table))

      val view        = View(base(ctx), uniqueId, tableName, columnNames)

      if (isOnlyOneRow) view.rowOpt(None, rows.headOption)
      else              view.many(rows)
    }

    def viewRow(ctx: String, id:ID): ViewFormat = {
      val selectQuery = query(table.filter(pk(_) === id))
      val rowOpt      = db.withSession(implicit s => databaseAction.read(base(ctx), primaryKeys, selectQuery, isEditable, max = Some(1)).headOption)
      val columnNames = QueryParser.columnNames(selectQuery)

      View(base(ctx), uniqueId, tableName, columnNames).rowOpt(Some(id).map(_.toString), rowOpt)
    }

    def update(ctx: String, id: ID, updates: Map[ColumnName, String]) = {
      val filteredTable = table.filter(pk(_) === id)
      db.withTransaction{ implicit s =>
        databaseAction.update(filteredTable, query(filteredTable), updates) match {
          case Left(fails: Seq[FailedUpdate]) =>
            s.rollback()
            fails foreach notifier.updateFailed(tableName, id)
            BadRequest ~> ResponseString(fails.mkString("\n"))
          case Right(okUpdates) =>
            okUpdates foreach notifier.updated(tableName, id)
            Ok ~> ResponseString(okUpdates.mkString("\n"))
        }
      }
    }
  }
}