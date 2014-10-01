package no.penger
package crud

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.xml.NodeSeq

trait editables extends namedCells with QueryParserModule {
  import profile.simple._

  @annotation.implicitNotFound("Couldnt find cell instances for all the types in projection ${PROJECTION}")
  trait Editable[PROJECTION]{

    def cells:List[Cell[_]]
    def list(e:PROJECTION):List[Any]
    def columns(q:Query[_, PROJECTION, Seq]):Seq[TableColumn] = QueryParser.columns(q)

    def namedCells(q:Query[_, PROJECTION, Seq]) =
      columns(q).zip(cells).map{
        case (cell, column) => NamedCell(cell, column)
      }

    /* fetches rows from db and renders them using the cells provided in cells() */
    def rows[T](ctx:      String,
                pk:       Set[TableColumn],
                q:        Query[T, PROJECTION, Seq],
                editable: Boolean,
                max:      Option[Int] = None)(implicit tx: Session): Seq[Seq[NodeSeq]] = {

      val rows  = max.fold(q)(n => q.take(n)).list
      val named = namedCells(q)

      rows.map { row =>
        named.zip(list(row)).map {
          case (cell, value) =>
            if (pk(cell.name)) cell.link(ctx, value)
            else if (editable) cell.editable(value)
            else               cell.fixed(value)
        }
      }
    }

    /* uses reflection to find all slick columns for a table, and then inspect the AST
     *  to find the one we want to update */
    def columnForCell[TABLE: ClassTag](table:TABLE, cell: NamedCell): Column[Any] = {
      import scala.reflect.runtime.universe._

      val mirror    = runtimeMirror(this.getClass.getClassLoader)
      val reflected = mirror.reflect(table)

      val methods   = reflected.symbol.asType.toType.members.collect {
        case m if m.typeSignature.resultType.typeConstructor =:= typeOf[slick.lifted.Column[Any]].typeConstructor => m.asMethod
      }

      val appliedMethods = methods.map(m => reflected.reflectMethod(m).apply())
      val allColumns     = appliedMethods.map(_.asInstanceOf[Column[Any]]).toArray

      val Column = cell.name.c

      allColumns.map(n => (n, n.toNode)).collectFirst{
        case (n,                       QueryParser.NamedColumn(Column))  => n
        case (n, slick.ast.OptionApply(QueryParser.NamedColumn(Column))) => n
      }.get
    }

    def update[TABLE](params: Map[String, Seq[String]],
                      q:      Query[TABLE, PROJECTION, Seq])
                     (implicit s:      Session,
                      c:      ClassTag[TABLE]): Either[Seq[Throwable], Int] = {

      val namedCellsForQuery: Seq[NamedCell] = namedCells(q)

      def namedCellForKey(key: String): Try[NamedCell] = {
        namedCellsForQuery.find(_.name.columnName == key) match {
          case Some(cell) => Success(cell)
          case None       => Failure(new RuntimeException(s"table ${QueryParser.tablenameFrom(q)} does not have a column $key"))
        }
      }

      val results: Iterable[Try[Int]] = for {
        (key, values) <- params
      } yield for {
          namedCell  <- namedCellForKey(key)
          validValue <- namedCell.tryCast(values.head)
          result     <- Try(q.map(cols => columnForCell(cols, namedCell)).update(validValue))
        } yield result

      sequence(results)
    }

    def sequence(result: Iterable[Try[Int]]): Either[Seq[Throwable], Int] =
      result.foldLeft[Either[Seq[Throwable], Int]](Right(0)){
        case (Right(acc), Success(saved)) => Right(acc + saved)
        case (Right(_),   Failure(why))   => Left(Seq(why))
        case (Left(acc),  Success(_))     => Left(acc)
        case (Left(acc),  Failure(saved)) => Left(acc :+ saved)
      }
  }
}
