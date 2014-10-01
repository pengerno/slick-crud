package no.penger.crud

import no.penger.db.SlickTransactionBoundary

import com.typesafe.scalalogging.slf4j.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request._
import unfiltered.response.{BadRequest, Ok, ResponseString}

import java.util.UUID
import scala.language.existentials
import scala.reflect.ClassTag
import scala.xml.NodeSeq

trait CRUD extends SlickTransactionBoundary {
  import profile.simple._

  /**
   * this is the main entry point, use this to expose database tables.
   *
   * @param table the table to be exposed for manipulation
   * @param mount the url through which the exposed table can be reached
   * @param query a function that maps a table query TQ to a query
   * @param key a function that maps a projection L to its id column to be used for updating one row
   * @tparam TQ slicks TableQuery
   * @tparam COLS the type of the lifted projection of a table, for example (Column[Int], Column[String])
   * @tparam ID the type of the primary key column
   * @tparam L the type of the (not case class) projection of a table, for example (Int, String)
   * @return
   */
  def Editor[TQ <: TableQuery[_], COLS : ClassTag, ID: Cell : BaseColumnType, L: Editable]
    (table:  TQ,
     mount:  String)
    (query:  TQ      => Query[COLS, L, Seq])
    (key:    COLS    => Column[ID]) = {

    new Ed[TQ, COLS, ID, L](table, mount, key, Nil, false, query(table))
  }

  case class Ed[TQ <: TableQuery[_], COLS : ClassTag, ID: Cell : BaseColumnType, L: Editable](
      table:   TQ,
      mount:   String,
      key:     COLS => Column[ID],
      editors: Seq[ID => Ed[_, _, _, _]],
      onlyOne: Boolean,
      query:   Query[COLS, L, Seq]
    ) extends LazyLogging {

    val editor   = implicitly[Editable[L]]
    val idCell   = implicitly[Cell[ID]]

    /* return a subeditor which is bound through a foreign key so that it can be referenced from another editor via sub() */
    def on[X : BaseColumnType](f:COLS => Column[X]) = (x:X) =>
      copy(query = query.filter(f(_) === x))

    /* return a new editor that also exposes other editors referenced via their primary key */
    def sub(editors:(ID => Ed[_, _, _, _])*) = copy(editors = editors)

    /* return a new editor that shows just one db row with a vertical table of columns */
    def single = copy(onlyOne = true)

    /* returns a set with the column names of all primary keys */
    lazy val pks: Set[String] = QueryParser.columns(query.map(key)).toSet

    /* name of table */
    lazy val tableName: String = QueryParser.tableName(query)

    lazy val uniqueId = tableName+UUID.randomUUID().toString.filter(_.isLetterOrDigit)

    case class script(ctx:String) {
      def single = <script type="text/javascript">FINN.pf.crud.single('{(ctx :: path).mkString("/")}', '#{uniqueId}')</script>
      def view   = <script type="text/javascript">FINN.pf.crud.view('{(ctx :: path).mkString("/")}', '#{uniqueId}')</script>
    }

    object Id {
      def unapply(parts:List[String]) =
        parts.splitAt(path.size) match {
          case (`path`, id :: Nil) => idCell.cast(id).right.toOption
          case _ => None
        }
    }

    val path = Seg.unapply(mount).get

    def intent:Plan.Intent = {
      case req@GET(ContextPath(ctx, Seg(`path`))) => transaction.readOnly{ implicit tx =>
        PageTemplate.page(ctx, "title"){
          view(ctx)
        }
      }

      case req@GET(ContextPath(ctx, Seg(Id(id)))) => transaction.readOnly{ implicit tx =>
        PageTemplate.page(ctx, "title"){
          viewsingle(ctx, id) ++ editors.flatMap(_(id).view(ctx))
        }
      }

      case req@POST(ContextPath(_, Seg(Id(id)))) & Params(params) => transaction.readWrite{ implicit tx =>
        update(id, params)
      }
    }

    def viewsingle(base:String, id:ID)(implicit s:Session) = {
      val selectQuery = query.filter(key(_) === id.bind)
      val rowOpt      = editor.rows((base:: path).mkString("/"), pks, selectQuery).headOption

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
      }.getOrElse(<h1>{"denne " + tableName + " finnes ikke"}</h1>)
        }</div>
    }

    def view(base:String)(implicit tx: Session) = {
      val rows = editor.rows((base :: path).mkString("/"), pks, query)
      <div>
        <h2>{tableName}</h2>{
        if(onlyOne)
          rows.headOption.map{ row =>
            script(base).single ++
              <table id={uniqueId}>
                <thead><tr><th>Column</th><th>Value</th></tr></thead>
                {editor.columns(query).zip(row).map{ case (name, value) => <tr><td>{name}</td>{value}</tr>}}
              </table>
          }.getOrElse(<h1>{"denne " + tableName + " finnes ikke"}</h1>)
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

  @annotation.implicitNotFound("Couldnt find cell instances for all the types in projection ${PROJECTION}")
  trait Editable[PROJECTION]{

    def cells:List[Cell[_]]
    def list(e:PROJECTION):List[Any]
    def columns(q:Query[_, PROJECTION, Seq]):List[String] = QueryParser.columns(q).toList

    def namedCells(q:Query[_, PROJECTION, Seq]) =
      columns(q).zip(cells).map{ case (cell, column) => NamedCell(cell, column) }

    /* fetches rows from db and renders them using the cells provided in cells() */
    def rows(base:String, pk:Set[String], q:Query[_, PROJECTION, Seq])(implicit tx: Session): List[Seq[NodeSeq]] = {
      val rows = q.list
      val named = namedCells(q)

      rows.map {
        row =>
          named.zip(list(row)).map {
            case (cell, value) =>
              if (pk(cell.name)) cell.link(base, value)
              else cell.editable(value)
          }
      }
    }

    def update[COLS](params: Map[String, Seq[String]],
                     q:      Query[COLS, PROJECTION, Seq])
           (implicit s:      Session,
                     c:      ClassTag[COLS]): Either[Seq[String], Int] = {

      val columnsQ: List[String] = columns(q)

      def save(cell:NamedCell, value: String): Either[String, Int] = {
        import scala.reflect.runtime.universe._

        val mirror = runtimeMirror(this.getClass.getClassLoader)

        def indexedColumn(p:COLS): Column[Any] = {

          val reflected = mirror.reflect(p)

          val methods = reflected.symbol.asType.toType.members.collect{
            case m if m.typeSignature.resultType.typeConstructor =:= typeOf[slick.lifted.Column[Any]].typeConstructor => m.asMethod
          }.toArray

          val appliedMethods = methods.map(m => reflected.reflectMethod(m).apply()).toArray

          val columns2 = appliedMethods.map(_.asInstanceOf[Column[Any]]).toArray

          val column = columns2.map(n => (n, n.toNode)).collectFirst{
            case (n, slick.ast.Select(_, slick.ast.FieldSymbol(name))) if name == cell.name => n
          }

          column.get
        }

        cell.cast(value).fold(
          f => Left(cell.name + " " +f),
          e => Right(q.map(indexedColumn).update(e)))
      }

      val namedCellsQ: List[NamedCell] = namedCells(q)

      val results = params.map {
        case (key, values) => namedCellsQ.find(_.name == key) match {
          case Some(namedCell) => save(namedCell, values.head)
          case _ => Left(s"table ${QueryParser.tableName(q)} does not have a column $key")
        }
      }
      Editable.sequence(results)
    }
  }

  /**
   * Somewhat lazy, but this was the easiest way to find column names for query without diving into the data structures.
   * Strictly supports query from one table
   */
  object QueryParser{
    val Cols          = """select (.*) from.*""".r
    val Colname       = """.+\."?(\w+)"?.*""".r
    val Tablename     = """.*from "?(\w+)"?.*""".r

    def columns(q: Query[_, _, Seq]): Seq[String] = columns(q.selectStatement)

    def columns(selectStatement: String): Seq[String] =
      selectStatement match {
        case Cols(cols) => cols.split(",").map(_.trim).filterNot(_.isEmpty).map{
          case Colname(name) => name
        }
      }

    def tableName(q: Query[_, _, Seq]): String = tableName(q.selectStatement)

    def tableName(selectStatement: String): String = selectStatement match {
      case Tablename(name) => name
    }
  }

  object Editable {

    def sequence(result: Iterable[Either[String, Int]]): Either[Seq[String], Int] =
      result.foldLeft[Either[Seq[String], Int]](Right(0)){
        case (Right(acc), Right(saved)) => Right(acc + saved)
        case (Left(acc), Left(saved))   => Left(acc :+ saved)
        case (left, Right(_))           => left
        case (_, Left(why))             => Left(Seq(why))
      }
  }

  /**
   * A Cell is the typeclass which takes care of mapping a type to/from the web.
   * link(), editable() and fixed() provides three different ways to render,
   * while cast() parses a string back to the given type so it can be persisted.
   */
  trait Cell[E]{
    def link(base:String, e:E):NodeSeq
    def editable(e:E):NodeSeq
    def fixed(e:E):NodeSeq
    def cast(value:String):Either[String, E]
  }

  object Cell {

    def from[A: Cell]: Cell[A] = implicitly[Cell[A]]

    import scala.util.control.Exception.allCatch

    /* use this constructor for easy cell creation if you dont need to customize rendering or error messages */
    def apply[T: Manifest](from: T => String, to: String => T, canEdit: Boolean = true, alignment: String = "right") = new Cell[T]{
      def link(base: String, e: T) = <td align={alignment}><a href={base + "/" + from(e)}>{from(e)}</a></td>
      def editable(e: T)           = <td contenteditable={canEdit.toString} align={alignment}>{from(e)}</td>
      def fixed(e: T)              = <td align="right">{from(e)}</td>
      def cast(value: String)      = allCatch.opt(to(value)).toRight(s"$value is not a valid ${implicitly[Manifest[T]].runtimeClass}")
    }

    /* implicitly provide handling of optional values*/
    implicit def optionCell[A: Cell] = new Cell[Option[A]] {
      val cell = implicitly[Cell[A]]

      def link(base: String, e: Option[A]) =
        e.map(v => cell.link(base, v)).getOrElse(fixed(e))

      def editable(e: Option[A]) =
        e.map(cell.editable).getOrElse(<td contenteditable="true" align="right"></td>)

      def fixed(e: Option[A]) =
        e.map(cell.fixed).getOrElse(<td align="right"></td>)

      def cast(value: String): Either[String, Option[A]] = {
        val v = value.trim
        if (v.isEmpty) Right(None) else cell.cast(value).right.map(Some(_))
      }
    }

    /* provided because of custom rendering */
    implicit val boolean = new Cell[Boolean] {
      import scala.xml._

      private def checked(elem: Elem, checked: Boolean) =
        if (checked) elem % Attribute("checked", Seq(Text("checked")), xml.Null) else elem

      def link(base: String, e: Boolean) = fixed(e)
      def editable(e: Boolean)           = <td>{checked(<input type="checkbox"/>, e)}</td>
      def fixed(e: Boolean)              = <td>{checked(<input type="checkbox" disabled="disabled"/>, e)}</td>
      def cast(value: String)            = allCatch.opt(value.toBoolean).toRight(value + " is not a boolean")
    }

    implicit lazy val doubleCell = apply[Double](_.toString, _.toDouble)
    implicit lazy val intCell    = apply[Int]   (_.toString, _.toInt)
    implicit lazy val stringCell = apply[String](identity,   identity)
  }

  /**
   * The purpose of this trait is to bring the cells to a more crude level of abstraction, where
   *  we leave behind the types of cells, and instead match them with db columns by the column name.
   *  In this sense these are the binding between string values from the web frontend and the database.
   *
   *  Things obviously still work, because we have already propagated cells (through Editable.cells())
   *   so validation and rendering is done properly.
   */
  trait NamedCell {
    def cast(s:String):Either[String, Any]
    def fixed(value:Any):NodeSeq
    def editable(value:Any):NodeSeq
    def link(base:String, value:Any):NodeSeq
    def name:String
  }

  def NamedCell(nme: String, cell: Cell[_]):NamedCell = new NamedCell {
    def cast(s: String): Either[String, Any] = cell.cast(s)
    def fixed(value: Any) = cell.asInstanceOf[Cell[Any]].fixed(value)
    def editable(value: Any) = cell.asInstanceOf[Cell[Any]].editable(value)
    def link(base: String, value: Any) = cell.asInstanceOf[Cell[Any]].link(base, value)
    def name = nme
  }
}
