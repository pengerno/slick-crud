package no.penger.crud

import slick.lifted.Rep

trait columnPicker extends astParser {

  /**
   * Given an abstract table, extract the lifted.Rep[_] with name 'name'.
   *
   * Note that the result cannot be reused/cached, as 'slickTable', and hence
   * the column we return, internally depends on the query in which it was generated
   */
  object findColumnWithName {
    def apply[TABLE <: AbstractTable[_]](slickTable: TABLE, name: ColumnName): Option[Rep[Any]] =
      columns(slickTable) collectFirst {
        case col if AstParser.colName(slickTable, col).name =:= name ⇒ col
      }

    /** To get at the columns here, we have to resort to reflection - I don't see any other way */
    def columns(slickTable: AbstractTable[_]): Iterable[Rep[Any]] = {
      val u         = scala.reflect.runtime.universe
      val mirror    = u.runtimeMirror(this.getClass.getClassLoader)
      val reflected = mirror.reflect(slickTable)

      def hasColumnReturnType(s: u.SymbolApi) =
        s.typeSignature.resultType.erasure =:= u.typeOf[Rep[Any]].erasure

      reflected.symbol.asType.toType.members.collect {
        case m if hasColumnReturnType(m) && m.isMethod ⇒
          reflected.reflectMethod(m.asMethod).apply().asInstanceOf[Rep[Any]]

        case m if hasColumnReturnType(m) && !m.isTerm ⇒
          reflected.reflectField(m.asTerm).get.asInstanceOf[Rep[Any]]
      }
    }
  }
}
