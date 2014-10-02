package no.penger
package crud

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait editables extends namedCells with queryParser {
  import profile.simple._

  @annotation.implicitNotFound("Couldnt find cell instances for all the types in projection ${PROJECTION}")
  trait Editable[PROJECTION]{

    def cells:List[Cell[_]]
    def list(e:PROJECTION):List[Any]
    def columns(q:Query[_, PROJECTION, Seq]):Seq[TableColumn] = QueryParser.columns(q)

    /* fetches rows from db and renders them using the cells provided in cells() */
    def rows[T](ctx:      String,
                pk:       Set[TableColumn],
                q:        Query[T, PROJECTION, Seq],
                editable: Boolean,
                max:      Option[Int] = None)
               (implicit tx: Session): Seq[Seq[ViewFormat]] = {

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

    def update[TABLE](params:     Map[String, Seq[String]],
                      q:          Query[TABLE, PROJECTION, Seq])
                     (implicit s: Session,
                               c: ClassTag[TABLE]): Either[Seq[FailedUpdate], Seq[Update]] = {

      val namedCellsForQuery: Seq[NamedCell] = namedCells(q)

      def namedCellForKey(key: String): Try[NamedCell] = {
        namedCellsForQuery.find(_.name.columnName == key) match {
          case Some(cell) => Success(cell)
          case None       => Failure(new RuntimeException(s"table ${QueryParser.tableNameFrom(q)} does not have a column $key"))
        }
      }

      val results: Iterable[Either[FailedUpdate, Update]] = for {
        (key, values) <- params
      } yield {
        val tried = for {
          namedCell   <- namedCellForKey(key)
          validValue  <- namedCell.tryCast(values.head)
          colToUpdate <- Try(q.map(columnForCell(q, namedCell)))
          oldValue    <- Try(colToUpdate.firstOption)
          numUpdates  <- Try(colToUpdate.update(validValue))
        } yield Update(key, oldValue, validValue, numUpdates)

        tried match {
          case Success(update) => Right(update)
          case Failure(t)      => Left(FailedUpdate(key, values, t))
        }
      }

      sequence(results)
    }

    private def namedCells(q:Query[_, PROJECTION, Seq]) =
      columns(q).zip(cells).map{
        case (cell, column) => NamedCell(cell, column)
      }

    private def columnForCell[TABLE: ClassTag](q: Query[TABLE, PROJECTION, Seq], cell: NamedCell)(row: TABLE): Column[Any] = {

      def nameOfColumn(c: Column[Any]): ColumnName = QueryParser.columns.columnsFor(c.toNode).head

      val allColumns: Seq[(Column[Any], ColumnName)] = (row, q.shaped.value) match {
          /* tuple projections */
        case (rowP: Product, qP: Product) =>
          rowP.productIterator.zip(qP.productIterator).map{
            /* this is messy. we need to inspect values from 'q' (because only they have the information we need),
                but we need to return the column from 'row' in order to not confuse slick.

                (The 'Column's from 'row' only contain references to the real columns, but with newly generated
                ids that do not match query and not any other information we have available here
                */
            case (rowCol, qCol) => (rowCol.asInstanceOf[Column[Any]], nameOfColumn(qCol.asInstanceOf[Column[Any]]))
          }.toSeq

        case (tableInstance, _) =>
          /* use reflection to find all slick columns for the slick.Table */
          import scala.reflect.runtime.universe._
          val mirror    = runtimeMirror(this.getClass.getClassLoader)
          val reflected = mirror.reflect(tableInstance)

          /* this was my best shot at getting at all the rows defined as defs and vals */
          val foundCols = reflected.symbol.asType.toType.members.collect {
            case m if m.typeSignature.resultType.typeConstructor =:= typeOf[slick.lifted.Column[Any]].typeConstructor =>

              if (m.isMethod) reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Column[Any]]
              else            reflected.reflectField(m.asTerm).get.asInstanceOf[Column[Any]]
          }

          foundCols.map(c => (c, nameOfColumn(c))).toSeq
      }

      allColumns.collectFirst{
        case (c, name) if name == cell.name.c => c
      }.get
    }
  }
}
