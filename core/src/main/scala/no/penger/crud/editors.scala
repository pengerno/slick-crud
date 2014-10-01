package no.penger
package crud

import java.util.UUID
import javax.servlet.http.HttpServletRequest

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.{HttpRequest, &, GET, POST, Params, Seg}
import unfiltered.response.{BadRequest, Ok, ResponseString}

import scala.reflect.ClassTag
import scala.xml.{Elem, NodeSeq}

trait editors extends editables with view {
  import profile.simple._

  object Editor{
    /**
     * this is the main entry point, use this to expose database tables.
     *
     * @param mount the url through which the exposed table can be reached
     * @param query a query on a table
     * @param key a function that maps a projection L to its id column to be used for updating one row
     * @tparam TABLE the type of the lifted projection of a table, for example (Column[Int], Column[String])
     * @tparam ID the type of the primary key column
     * @tparam PROJ the type of the projection of a table, for example (Int, String)
     */
    def apply[TABLE : ClassTag, ID: Cell : BaseColumnType, PROJ: Editable]
    (query:     Query[TABLE, PROJ, Seq],
     mount:     String,
     editable:  Boolean = true)
    (key:       TABLE => Column[ID]) = new Editor[TABLE, ID, PROJ](query, mount, key, Nil, editable, onlyOne = false)
  }

  case class Editor[TABLE : ClassTag, ID: Cell : BaseColumnType, L: Editable] private (
      query:   Query[TABLE, L, Seq],
      mount:   String,
      key:     TABLE  => Column[ID],
      editors: Seq[ID => Editor[_, _, _]],
      editable: Boolean,
      onlyOne: Boolean
    ) extends LazyLogging {

    val editor = implicitly[Editable[L]]
    val idCell = implicitly[Cell[ID]]

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:TABLE => Column[X]) = (x:X) => copy(query = query.filter(f(_) === x))

    /* return a new editor that also exposes other editors referenced via their primary key */
    def sub(editors:(ID => Editor[_, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(onlyOne = true)

    /* returns a set with the column names of all primary keys */
    lazy val pks: Set[String] = QueryParser.primaryKeys(query.map(key))

    /* name of table */
    lazy val tableName: String = QueryParser.tablenameFrom(query)

    val MountedAt = Seg.unapply(mount).get
    
    def ctxPath[T <: HttpServletRequest](req: HttpRequest[T]) = ContextPath.unapply(req).map(_._1).get

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    lazy val uniqueId = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(MountedAt.size) match {
          case (MountedAt, id :: Nil) => idCell.tryCast(id).toOption.map(i => (MountedAt, i))
          case _ => None
        }
    }

    def intent:Plan.Intent = {
      case req@GET(ContextPath(_, Seg(MountedAt))) => transaction.readOnly{ implicit tx =>
        presentPage(req, title = MountedAt.head){ view(req)}
      }

      case req@GET(ContextPath(_, Seg(Id(p, id)))) => transaction.readOnly{ implicit tx =>
        presentPage(req, title = s"${p.head} for $id"){
          viewSingle(req, id) ++ editors.flatMap(_(id).view(req))
        }
      }

      case req@POST(ContextPath(_, Seg(Id(_, id)))) & Params(params) => transaction.readWrite{ implicit tx =>
        update(id, params)
      }
    }

    def view[T <: HttpServletRequest](req: HttpRequest[T])(implicit tx: Session): Elem = {
      val rows: Seq[Seq[NodeSeq]] = editor.rows(ctxPath(req), pks, query, editable)

      if (onlyOne)
        rows.headOption match {
          case None      => WebIntegration.view404(tableName, None)
          case Some(row) => WebIntegration.single(ctxPath(req), uniqueId, tableName, editor.columns(query).zip(row))
        }
      else WebIntegration.many(ctxPath(req), uniqueId, tableName, rows, editor.columns(query))
    }

    def viewSingle[T <: HttpServletRequest](req: HttpRequest[T], id:ID)(implicit s:Session): Elem = {
      val selectQuery = query.filter(key(_) === id.bind)
      val rowOpt      = editor.rows(ctxPath(req), pks, selectQuery, editable).headOption
      
      rowOpt match {
        case None      => WebIntegration.view404(tableName, Some(id.toString))
        case Some(row) => WebIntegration.single(ctxPath(req), uniqueId, tableName, editor.columns(selectQuery).zip(row))
      }
    }

    def update(i:ID, params:Map[String, Seq[String]])(implicit tx: Session) =
      editor.update(params, query.filter(key(_) === i)) match {
        case Left(fails) =>
          tx.rollback()
          logger.warn(s"could not update ${this.tableName} with data $params: $fails")
          BadRequest ~> ResponseString(fails.mkString("\n"))
        case Right(updates) =>
          logger.info(s"updated $updates rows for table ${this.tableName} for id $i with values $params")
          Ok ~> ResponseString(updates + " rows updated")
      }
  }
}
