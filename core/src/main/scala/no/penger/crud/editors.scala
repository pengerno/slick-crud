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
trait editors extends editables with view with updateNotifier {

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
    def apply[ROW <: AbstractTable[_], LP : ClassTag, P : Editable, ID: Cell : BaseColumnType]
      (mount:      String,
       table:      TableQuery[ROW],
       notifier:   UpdateNotifier     = new UpdateNotifier,
       isEditable: Boolean            = true)
      (query:      TableQuery[ROW] => Query[LP, P, Seq])
      (pk:         LP => Column[ID]) =

      new Editor[LP, P, ID](mount, query(table), pk, notifier, isEditable, editors = Nil, isOnlyOneRow = false)
  }

  case class Editor[LP : ClassTag, P: Editable, ID: Cell : BaseColumnType] private (
      mount:        String,
      query:        Query[LP, P, Seq],
      pk:           LP  => Column[ID],
      notifier:     UpdateNotifier,
      isEditable:   Boolean,
      editors:      Seq[ID => Editor[_, _, _]],
      isOnlyOneRow: Boolean) {

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:LP => Column[X])(x:X) = copy(query = query.filter(f(_) === x))

    /* return a new editor with referenced subeditors */
    def sub(editors:(ID => Editor[_, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(isOnlyOneRow = true)

    val primaryKeys = QueryParser.primaryKeys(query.map(pk))
    val tableName   = QueryParser.tableNameFrom(query)
    
    val editable    = implicitly[Editable[P]]

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
      case req@GET(ContextPath(ctx, Seg(MountedAt))) => db.withSession{ implicit s =>
        respond(ctx, title = MountedAt.head)(view(ctx))
      }

      case req@GET(ContextPath(ctx, Seg(Id(id)))) => db.withSession{ implicit s =>
        respond(ctx, title = s"$tableName for $id")(
          editors.map(_(id).view(ctx)).foldLeft(viewRow(ctx, id))(append)
        )
      }

      case req@POST(ContextPath(_, Seg(Id(id)))) & Params(params) => db.withTransaction{ implicit tx =>
        update(id, params)
      }
    }

    def view(ctx: String)(implicit s: Session): ViewFormat = {
      val rows        = editable.rows(base(ctx), primaryKeys, query, isEditable, max = Some(1).filter(_ => isOnlyOneRow))
      val columnNames = editable.columns(query)
      val ed          = View(base(ctx), uniqueId, tableName)

      if (isOnlyOneRow) ed.rowOpt(None, rows.headOption, columnNames)
      else              ed.many(rows, columnNames)
    }

    def viewRow(ctx: String, id:ID)(implicit s:Session): ViewFormat = {
      val selectQuery = query.filter(pk(_) === id.bind)
      val rowOpt      = editable.rows(base(ctx), primaryKeys, selectQuery, isEditable, max = Some(1)).headOption
      val columnNames = editable.columns(selectQuery)
      
      View(base(ctx), uniqueId, tableName).rowOpt(Some(id).map(_.toString), rowOpt, columnNames)
    }

    def update(id: ID, params: Map[String, Seq[String]])(implicit s: Session) =
      editable.update(params, query.filter(pk(_) === id)) match {
        case Left(fails: Seq[FailedUpdate]) =>
          s.rollback()
          fails foreach notifier.updateFailed(tableName, id)
          BadRequest ~> ResponseString(fails.mkString("\n"))
        case Right(updates) =>
          updates foreach notifier.updated(tableName, id)
          Ok ~> ResponseString(updates + " rows updated")
      }
  }
}