package no.penger.crud

import scala.slick.lifted.AbstractTable
import scala.util.Try

trait crudActions extends queryParser with namedCells {
  import profile.simple._

  object databaseAction {
    /* fetches rows from db and renders them using the cells provided in cells() */
    def read[T, PROJECTION](ctx:      String,
                            pks:      Set[TableColumn],
                            q:        Query[T, PROJECTION, Seq],
                            editable: Boolean,
                            max:      Option[Int] = None)
                  (implicit s:        Session,
                            cr:       CellRow[PROJECTION]): Seq[Seq[ViewFormat]] = {

      val rows  = max.fold(q)(n => q.take(n)).list
      val named = namedCells(q)

      rows.map { row =>
        named.zip(cr.list(row)).map {
          case (cell, value) =>
            if (pks(cell.name)) cell.link(ctx, value)
            else if (editable)  cell.editable(value)
            else                cell.fixed(value)
        }
      }
    }

    def update(verifyQ: Q,
               updateQ: Q,
               updates: Map[ColumnName, String])
     (implicit s:       Session,
               cr:      CellRow[_]): Either[Seq[FailedUpdate], Seq[Update]] = {

      val named = namedCells(verifyQ)

      val results: Iterable[Either[FailedUpdate, Update]] = updates.map {
        case (columnName, value) =>
          val tried: Try[Update] = for {
            cellWithName   <- named.find(_.name.c == columnName).toTry(s"table ${QueryParser.tableNameFrom(updateQ)} does not have a column $columnName")
            columnWithName <- Try(updateQ.map(row => columnFromRowWithName(updateQ, row, cellWithName.name.c)))
            oldValue       <- Try(columnWithName.firstOption)
            validValue     <- cellWithName.tryCast(value)
            numUpdates     <- Try(columnWithName.update(validValue))
          } yield Update(columnName, oldValue, validValue, numUpdates)

          tried.toEither.left.map {
            case t => FailedUpdate(columnName, value, t)
          }
      }

      sequence(results)
    }

    private def namedCells(q: Q)(implicit e: CellRow[_]): Seq[NamedCell] =
      QueryParser.columnNames(q).zip(e.cells).map{
        case (columnName, cell) => NamedCell(columnName, cell)
      }


    /**
     * Given a row from a query, extract the lifted.Column[_] with name 'name'
     */
    private def columnFromRowWithName[R](q: Query[R, _, Seq], row: R, name: ColumnName): Column[Any] = {
      val rowsWithNames = row match {
        /* a projection to a tuple */
        case p:          Product          => ExtractColumnFromProductProjection(q, p)
        case slickTable: AbstractTable[_] => ExtractColumnsFromSlickTable(slickTable)
      }

      rowsWithNames collectFirst {
        case (col, colName) if colName == name => col
      } getOrElse {
        throw new RuntimeException(s"Couldn't find column $name in rows $rowsWithNames from query $q")
      }
    }

    object ExtractColumnFromProductProjection {
      val ExtractColName = """select (\w+\.)?(\w*) from.*""".r

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
            val ExtractColName(_, colname) = qq.selectStatement
            (c, ColumnName(colname))
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

        /* this was my best shot at getting at all the rows defined as defs and vals */
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