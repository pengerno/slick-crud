package no.penger.crud

import scala.slick.lifted.AbstractTable
import scala.util.Try

trait crudActions extends queryParser with cells {
  import profile.simple._

  object crudAction {
    /* fetches rows from db and renders them using the cells provided in cells() */
    def read[T, PROJECTION](ctx:      String,
                            pks:      Set[ColumnName],
                            q:        Query[T, PROJECTION, Seq],
                            editable: Boolean,
                            max:      Option[Int] = None)
                  (implicit s:        Session,
                            cr:       CellRow[PROJECTION]): Seq[Seq[ElemFormat]] = {

      val rows         = max.fold(q)(n => q.take(n)).list
      val untypedCells = untypedCellsForQuery(q)

      rows.map { row =>
        untypedCells.zip(cr.unpackValues(row)).map {
          case ((name, cell), value) =>
            if (pks(name))      cell.link(ctx, value)
            else if (editable)  cell.editable(value)
            else                cell.fixed(value)
        }.toIndexedSeq
      }
    }

    def update[T, ROW](q:       Query[T, ROW, Seq],
                       updates: Map[ColumnName, String])
             (implicit s:       Session,
                       cr:      CellRow[ROW]): Either[Seq[FailedUpdate], Seq[Update]] = {

      val untypedCells = untypedCellsForQuery(q)

      val results: Iterable[Either[FailedUpdate, Update]] = updates.map {
        case (columnName, value) =>
          val tried: Try[Update] = for {
            cell           <- untypedCells.collectFirst {
              case (`columnName`, cell) => cell
            }.toTry(s"table ${QueryParser.tableNameFrom(q)} does not have a column $columnName")
            updater        <- Try(q.map(row => columnFromRowWithName(q, row, columnName)))
            oldValue       <- Try(updater.first)
            validValue     <- cell.tryCast(value)
            numUpdates     <- Try(updater.update(validValue))
          } yield Update(columnName, oldValue, validValue, numUpdates)

          tried.toEither.left.map {
            case t => FailedUpdate(columnName, value, t)
          }
      }

      sequence(results)
    }

    def create[ROW <: AbstractTable[_], T, ID](table:   Query[ROW, ROW#TableElementType, Seq],
                                               id:      ROW ⇒ Column[ID],
                                               params:  Map[ColumnName, String])
                                     (implicit ev1:     BaseColumnType[ID],
                                               s:       Session,
                                               cr:      CellRow[ROW#TableElementType]): Either[Seq[Throwable], Option[ID]] = {

      val untypedCells: Seq[(ColumnName, Cell[Any])] = untypedCellsForQuery(table)

      val validatedValues: Seq[Either[Throwable, Any]] = untypedCells map {
        case (columnName, cell) ⇒
          val tried: Try[Any] = for {
            value      ← params.get(columnName).toTry(s"didn't provide value for $columnName")
            validValue ← cell.tryCast(value)
          } yield validValue

          tried.toEither
      }

      sequence(validatedValues).right.flatMap {
        validValues ⇒ {
          val inserter = table.returning(table.map(r ⇒ id(r).?))
          val toInsert = cr.packValues(validValues)
          val ret      = Try(inserter.insert(toInsert))
          ret match {
            case util.Success(oid) ⇒ Right(oid)
            case util.Failure(t)   ⇒
              /* try again without return of autoincrement value*/
              Try(table.insert(toInsert)) match {
                case util.Success(_) ⇒ Right(None)
                case util.Failure(t) ⇒ Left(Seq(t))
              }
          }
        }
      }
    }

    private def untypedCellsForQuery(q: Q)(implicit e: CellRow[_]): Seq[(ColumnName, Cell[Any])] =
      QueryParser.columnNames(q).map(_.c).zip(e.cells).map {
        case (colName, cell) => (colName, cell.asInstanceOf[Cell[Any]])
      }

    /**
     * Given a row from a query, extract the lifted.Column[_] with name 'name'
     */
    private def columnFromRowWithName[R](q: Query[R, _, Seq], row: R, name: ColumnName): Column[Any] = {
      val rowsWithNames: Seq[(Column[Any], ColumnName)] = row match {
        /* a projection to a tuple */
        case p:          Product          => ExtractColumnFromProductProjection(q, p)
        case slickTable: AbstractTable[_] => ExtractColumnsFromSlickTable(slickTable)
        case c: Column[_]                 => ExtractSoloColumn(q, c)
      }

      rowsWithNames collectFirst {
        case (col, colName) if colName == name => col
      } getOrElse {
        throw new RuntimeException(s"Couldn't find column $name in rows $rowsWithNames from query $q")
      }
    }

    trait ColumnTextExtractor{
      val ExtractColName = """select (\w+\.)?(\w*) from.*""".r
    }

    /* if there is only one column in the query this is what we get */
    object ExtractSoloColumn extends ColumnTextExtractor {
      def apply(q: Q, c: Column[_]) = {
        val ExtractColName(_, colName) = q.selectStatement
        Seq((c.asInstanceOf[Column[Any]], ColumnName(colName)))
      }
    }

    object ExtractColumnFromProductProjection extends ColumnTextExtractor {
      /**
       * This was particularly difficult to figure out. The 'Product' is a TupleN of Column[_]s.
       *
       * The 'Column's do not contain names, just references to 'Column's defined elsewhere, and I
       *  couldn't figure out a way to get at those directly.
       *
       *  What we're doing instead, is to create queries for all the columns, and extract the column
       *   name from that query. This obviously works, because slick has access to everything.
       *
       *  A 'Column' taken from a 'Query' doesn't make sense without its 'Query', which is why
       *   we need to take care here to create new queries for every selectStatement, and
       *   to return 'Column's coming from 'row'
       */
      def apply(q: Q, row: Product): Seq[(Column[Any], ColumnName)] =
        row.productIterator.map(_.asInstanceOf[Column[Any]]).zipWithIndex.map {
          case (c, idx) =>
            val qq = q.map(_.asInstanceOf[Product].productElement(idx).asInstanceOf[Column[Any]])
            val ExtractColName(_, colName) = qq.selectStatement
            (c, ColumnName(colName))
        }.toSeq
    }

    object ExtractColumnsFromSlickTable {
      /**
       *  To get at the columns here, we have to resort to reflection - I don't see any other way
       *
       *  This code will only work for columns defined as vals and defs without any parameters (the presence
       *   of one with parameters will probably make this blow up)
       */
      def apply(slickTable: AbstractTable[_]): Seq[(Column[Any], ColumnName)] = {

        import scala.reflect.runtime.universe._
        val mirror    = runtimeMirror(this.getClass.getClassLoader)
        val reflected = mirror.reflect(slickTable)

        def nameOfColumn(c: Column[Any]): ColumnName = QueryParser.columnNames.columnsFor(c.toNode).head

        /* this was my best shot at getting at all the columns defined as defs and vals */
        val foundCols = reflected.symbol.asType.toType.members.collect {
          case m if m.typeSignature.resultType.typeConstructor =:= typeOf[slick.lifted.Column[Any]].typeConstructor =>

            if (m.isMethod) reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Column[Any]]
            else            reflected.reflectField(m.asTerm).get.asInstanceOf[Column[Any]]
        }

        foundCols.toSeq.map(c => (c, nameOfColumn(c)))
      }
    }
  }
}