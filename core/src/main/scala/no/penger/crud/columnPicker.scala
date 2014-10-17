package no.penger.crud

import scala.slick.lifted.{AbstractTable, Column}

trait columnPicker extends queryParser {

  /**
   * Given an abstract table, extract the lifted.Column[_] with name 'name'
   */
  object ColumnWithName {
    def apply[T <: AbstractTable[_]](name: ColumnName)(slickTable: T): Column[Any] = {
      val cols: Seq[Column[Any]] = ExtractColumnsFromSlickTable(slickTable)

      cols collectFirst {
        case col if nameOfColumn(col) =:= name ⇒ col
      } getOrElse {
        throw new RuntimeException(s"Couldn't find column $name")
      }
    }

    def nameOfColumn(c: Column[Any]): ColumnName = QueryParser.columnNames.columnsFor(c.toNode).head

    object ExtractColumnsFromSlickTable {
      /** To get at the columns here, we have to resort to reflection - I don't see any other way */
      def apply(slickTable: AbstractTable[_]): Seq[Column[Any]] = {

        import scala.reflect.runtime.universe._
        val mirror    = runtimeMirror(this.getClass.getClassLoader)
        val reflected = mirror.reflect(slickTable)

        /* this was my best shot at getting at all the columns defined as defs and vals */
        val foundCols: Iterable[Column[Any]] = reflected.symbol.asType.toType.members.collect {
          case m if m.typeSignature.resultType.typeConstructor =:= typeOf[slick.lifted.Column[Any]].typeConstructor ⇒

            if (m.isMethod) reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Column[Any]]
            else            reflected.reflectField(m.asTerm).get.asInstanceOf[Column[Any]]
        }

        foundCols.toSeq
      }
    }
  }
}
