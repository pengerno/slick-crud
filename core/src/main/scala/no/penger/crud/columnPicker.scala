package no.penger.crud

import scala.reflect.api
import scala.slick.lifted.Column

trait columnPicker extends astParser {

  /**
   * Given an abstract table, extract the lifted.Column[_] with name 'name'
   */
  object findColumnWithName {
    def apply[T <: AbstractTable[_]](slickTable: T, name: ColumnName): Option[Column[Any]] =
      ExtractColumnsFromSlickTable(slickTable) collectFirst {
        case col if AstParser.colName(col) =:= name ⇒ col
      }

    object ExtractColumnsFromSlickTable {
      /** To get at the columns here, we have to resort to reflection - I don't see any other way */
      def apply(slickTable: AbstractTable[_]): Seq[Column[Any]] = {

        val u: api.JavaUniverse = scala.reflect.runtime.universe

        val mirror    = u.runtimeMirror(this.getClass.getClassLoader)
        val reflected = mirror.reflect(slickTable)

        def hasColumnReturnType(s: u.SymbolApi) =
          s.typeSignature.resultType.erasure =:= u.typeOf[Column[Any]].erasure

        val foundCols: Iterable[Column[Any]] = reflected.symbol.asType.toType.members.collect {
          case m if hasColumnReturnType(m) && m.isMethod ⇒
            reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Column[Any]]

          case m if hasColumnReturnType(m) && !m.isTerm ⇒
            reflected.reflectField(m.asTerm).get.asInstanceOf[Column[Any]]
        }

        foundCols.toSeq
      }
    }
  }
}
