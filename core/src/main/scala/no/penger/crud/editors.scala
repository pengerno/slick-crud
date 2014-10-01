package no.penger
package crud

import java.util.UUID

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.{&, GET, POST, Params, Seg}
import unfiltered.response.{BadRequest, Ok, ResponseString}

import scala.reflect.ClassTag
import scala.xml.NodeSeq

trait editors extends editables with view[NodeSeq] {
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
    lazy val tablename: String = QueryParser.tablenameFrom(query)

    val MountedAt = Seg.unapply(mount).get

    def base(ctx: String) = (ctx:: MountedAt).mkString("/")

    /* generate a random id for the table we render, for frontend to distinguish multiple tables */
    lazy val uniqueId = tablename+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(MountedAt.size) match {
          case (MountedAt, id :: Nil) => idCell.tryCast(id).toOption.map(i => (MountedAt, i))
          case _ => None
        }
    }

    def intent:Plan.Intent = {
      case req@GET(ContextPath(ctx, Seg(MountedAt))) => transaction.readOnly{ implicit tx =>
        respond(ctx, title = MountedAt.head){ view(ctx)}
      }

      case req@GET(ContextPath(ctx, Seg(Id(p, id)))) => transaction.readOnly{ implicit tx =>
        respond(ctx, title = s"${p.head} for $id"){
          viewSingle(ctx, id) ++ editors.flatMap(_(id).view(ctx))
        }
      }

      case req@POST(ContextPath(_, Seg(Id(_, id)))) & Params(params) => transaction.readWrite{ implicit tx =>
        update(id, params)
      }
    }

    def view(ctx: String)(implicit tx: Session): NodeSeq = {
      val rows        = editor.rows(base(ctx), pks, query, editable, max = Some(1).filter(_ => onlyOne))
      val columnNames = editor.columns(query)

      if (onlyOne)
        newEditor(base(ctx), uniqueId, tablename).rowOpt(None, rows.headOption, columnNames)
      else
        newEditor(base(ctx), uniqueId, tablename).many(rows, columnNames)
    }

    def viewSingle(ctx: String, id:ID)(implicit s:Session): NodeSeq = {
      val selectQuery = query.filter(key(_) === id.bind)
      val rowOpt      = editor.rows(base(ctx), pks, selectQuery, editable, max = Some(1)).headOption
      val columnNames = editor.columns(selectQuery)
      
      newEditor(base(ctx), uniqueId, tablename).rowOpt(Some(id).map(_.toString), rowOpt, columnNames)
    }

    def update(i:ID, params:Map[String, Seq[String]])(implicit tx: Session) =
      editor.update(params, query.filter(key(_) === i)) match {
        case Left(fails) =>
          tx.rollback()
          logger.warn(s"could not update ${this.tablename} with data $params: $fails")
          BadRequest ~> ResponseString(fails.mkString("\n"))
        case Right(updates) =>
          logger.info(s"updated $updates rows for table ${this.tablename} for id $i with values $params")
          Ok ~> ResponseString(updates + " rows updated")
      }
  }
}
