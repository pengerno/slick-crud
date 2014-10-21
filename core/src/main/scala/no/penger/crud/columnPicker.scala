package no.penger.crud

import scala.reflect.api
import scala.slick.lifted.{AbstractTable, Column}

trait columnPicker extends queryParser {

  /**
   * Given an abstract table, extract the lifted.Column[_] with name 'name'
   */
  object ColumnWithName {
    def apply[T <: AbstractTable[_]](name: ColumnName)(slickTable: T): Option[Column[Any]] =
      ExtractColumnsFromSlickTable(slickTable) collectFirst {
        case col if nameOfColumn(col) =:= name ⇒ col
      }

    def nameOfColumn(c: Column[Any]): ColumnName = QueryParser.columnNames.columnsFor(c.toNode).head

    object ExtractColumnsFromSlickTable {
      /** To get at the columns here, we have to resort to reflection - I don't see any other way */
      def apply(slickTable: AbstractTable[_]): Seq[Column[Any]] = {

        val u: api.JavaUniverse = scala.reflect.runtime.universe

        val mirror    = u.runtimeMirror(this.getClass.getClassLoader)
        val reflected = mirror.reflect(slickTable)

        def isNullarySlickColumn(s: u.SymbolApi) = {
          val nullaryColumn = s.typeSignature.typeConstructor match {
            case nullary: u.NullaryMethodTypeApi => Some(nullary.resultType.typeConstructor)
            case _                               => None
          }
          nullaryColumn =:= Some(u.typeOf[Column[Any]].typeConstructor)
        }

        val foundCols: Iterable[Column[Any]] = reflected.symbol.asType.toType.members.collect {
          case m if isNullarySlickColumn(m) && m.isMethod ⇒
            reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Column[Any]]

          case m if isNullarySlickColumn(m) && !m.isTerm ⇒
            reflected.reflectField(m.asTerm).get.asInstanceOf[Column[Any]]
        }

        foundCols.toSeq
      }
    }
  }
}
