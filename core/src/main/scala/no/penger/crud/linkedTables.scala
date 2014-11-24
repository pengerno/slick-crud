package no.penger.crud

trait linkedTables extends tableRefs with crudActions with renderers {

  /**
   * A foreign key read mapping parametrized on the row of the base tables row projection type
   */
  sealed abstract class LinkedTable[P] {
    def view(foundRow: P): PageFormat
  }

  /**
   * This is essentially a partially applied foreign key mapping query
   *  where we package up the types and details. `foundRow` is injected
   *  only when we display the row in question
   */
  case class LinkedTableImpl[P, COL, OID: Cell, OTABLE <: AbstractTable[_], OLP, OP]
                              (colValue:          P   ⇒ COL,
                               queryFromColValue: COL ⇒ FilteredTableRef[OID, OTABLE, OLP, OP, COL]) extends LinkedTable[P] {
    def view(foundRow: P) = {
      val ref = queryFromColValue(colValue(foundRow))
      crudAction.read(ref.query).zipMap(ref.extractIdFromRow) match {
        case Nil              ⇒ Renderer(ref) newRow Some((ref.colName, ref.colValue))
        case (id, row) :: Nil ⇒ Renderer(ref) row (id, row)
        case idsRows          ⇒ Renderer(ref) rows idsRows
      }
    }
  }
}
