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
 *  - render responses through an 'EditorView'
 *  - talks to the database through an 'Editable'
 *  - renders and parses values through 'Cell's
 *  - sends notifications of (failed) updates thorough an 'UpdateNotifier'
 *  - composes with other 'Editor's (see 'on()', 'single()' and 'sub()')
 */
trait editors extends editables with view with updateNotifier {

  def respond(base: String, title: String)(body: ViewFormat): ResponseFunction[Any]

  import profile.simple._

  object Editor{
    /**
     * @param mount the url through which the exposed table can be reached
     * @param query a query on a table
     * @param key a function that maps a projection L to its id column to be used for updating one row
     * @tparam TABLE the type of the lifted projection of a table, for example (Column[Int], Column[String])
     * @tparam ID the type of the primary key column
     * @tparam PROJ the type of the projection of a table, for example (Int, String)
     */
    def apply[TABLE : ClassTag, ID: Cell : BaseColumnType, PROJ: Editable]
      (query:    Query[TABLE, PROJ, Seq],
       mount:    String,
       notifier: UpdateNotifier = new UpdateNotifier,
       editable: Boolean        = true)
      (key:      TABLE => Column[ID]) =

        new Editor[TABLE, ID, PROJ](query, mount, key, Nil, editable, isOnlyOneRow = false, notifier)
  }

  case class Editor[TABLE : ClassTag, ID: Cell : BaseColumnType, L: Editable] private (
      query:        Query[TABLE, L, Seq],
      mount:        String,
      key:          TABLE  => Column[ID],
      editors:      Seq[ID => Editor[_, _, _]],
      isEditable:   Boolean,
      isOnlyOneRow: Boolean,
      notifier:     UpdateNotifier) {

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:TABLE => Column[X])(x:X) = copy(query = query.filter(f(_) === x))

    /* return a new editor that also exposes other editors referenced via their primary key */
    def sub(editors:(ID => Editor[_, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(isOnlyOneRow = true)

    val primaryKeys = QueryParser.primaryKeys(query.map(key))
    val tableName   = QueryParser.tableNameFrom(query)
    
    val editable    = implicitly[Editable[L]]

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
      val ed          = EditorView(base(ctx), uniqueId, tableName)

      if (isOnlyOneRow) ed.rowOpt(None, rows.headOption, columnNames)
      else              ed.many(rows, columnNames)
    }

    def viewRow(ctx: String, id:ID)(implicit s:Session): ViewFormat = {
      val selectQuery = query.filter(key(_) === id.bind)
      val rowOpt      = editable.rows(base(ctx), primaryKeys, selectQuery, isEditable, max = Some(1)).headOption
      val columnNames = editable.columns(selectQuery)
      
      EditorView(base(ctx), uniqueId, tableName).rowOpt(Some(id).map(_.toString), rowOpt, columnNames)
    }

    def update(id: ID, params: Map[String, Seq[String]])(implicit s: Session) =
      editable.update(params, query.filter(key(_) === id)) match {
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