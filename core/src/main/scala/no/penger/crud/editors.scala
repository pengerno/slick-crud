package no.penger
package crud

import java.util.UUID

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.{&, GET, HttpRequest, POST, Params, Seg}
import unfiltered.response.{BadRequest, Ok, ResponseFunction, ResponseString}

import scala.reflect.ClassTag
import scala.xml.NodeSeq

trait editors extends editables {
  import profile.simple._

  def presentPage[T](req: HttpRequest[T], ctx: String, title: String)(body: NodeSeq): ResponseFunction[Any]

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
    def on[X : BaseColumnType](f:TABLE => Column[X]) = (x:X) =>
      copy(query = query.filter(f(_) === x))

    /* return a new editor that also exposes other editors referenced via their primary key */
    def sub(editors:(ID => Editor[_, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(onlyOne = true)

    /* returns a set with the column names of all primary keys */
    lazy val pks: Set[String] = QueryParser.primaryKeys(query.map(key))

    /* name of table */
    lazy val tableName: String = QueryParser.tablenameFrom(query)

    lazy val uniqueId = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    case class script(ctx:String) {
      def single = <script type="text/javascript">no.penger.crud.single('{(ctx :: path).mkString("/")}', '#{uniqueId}')</script>
      def view   = <script type="text/javascript">no.penger.crud.view('{(ctx :: path).mkString("/")}', '#{uniqueId}')</script>
    }

    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(path.size) match {
          case (`path`, id :: Nil) => idCell.tryCast(id).toOption.map(i => (path, i))
          case _ => None
        }
    }

    val path = Seg.unapply(mount).get

    def intent:Plan.Intent = {
      case req@GET(ContextPath(ctx, Seg(`path`))) => transaction.readOnly{ implicit tx =>
        presentPage(req, ctx, path.head){
          view(ctx)
        }
      }

      case req@GET(ContextPath(ctx, Seg(Id(p, id)))) => transaction.readOnly{ implicit tx =>
        presentPage(req, ctx, s"${p.head} for $id"){
          viewsingle(ctx, id) ++ editors.flatMap(_(id).view(ctx))
        }
      }

      case req@POST(ContextPath(_, Seg(Id(_, id)))) & Params(params) => transaction.readWrite{ implicit tx =>
        update(id, params)
      }
    }

    def viewsingle(base:String, id:ID)(implicit s:Session) = {
      val selectQuery = query.filter(key(_) === id.bind)
      val rowOpt      = editor.rows((base:: path).mkString("/"), pks, selectQuery, editable).headOption

      <div>
        <h2>{tableName}</h2>
        {rowOpt.map { row =>
        script(base).single ++
          <table id={uniqueId}>
            <thead><tr><th>Column</th><th>Value</th></tr></thead>
            {editor.columns(selectQuery).zip(row).map{
              case (name, value) => <tr><td>{name}</td>{value}</tr>
            }}
          </table>
      }.getOrElse(<h1>{s"Could not find a $tableName for $id"}</h1>)
        }</div>
    }

    def view(base:String)(implicit tx: Session) = {
      val rows = editor.rows((base :: path).mkString("/"), pks, query, editable)
      <div>
        <h2>{tableName}</h2>{
        if(onlyOne)
          rows.headOption.map{ row =>
            script(base).single ++
              <table id={uniqueId}>
                <thead><tr><th>Column</th><th>Value</th></tr></thead>
                {editor.columns(query).zip(row).map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
              </table>
          }.getOrElse(<h1>{s"Could not find this $tableName"}</h1>)
        else
          script(base).view ++
            <table id={uniqueId}>
              <thead><tr>{editor.columns(query).map(name => <th>{name}</th>)}</tr></thead>
              {rows.map{ row => <tr>{row}</tr>}}
            </table>}
      </div>
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
