package no.penger.crud

trait linkedTables extends tableRefs with crudActions with renderers {

  /**
   * A foreign key read mapping parametrized on the base tables' row projection type
   */
  sealed abstract class LinkedTable[P] {
    def view(foundRow: P): PageFormat
  }

  /**
   * This is essentially a partially applied foreign key mapping query
   *  where we package up and hide types and query details. `foundRow` is injected
   *  only when we display the row in question
   */
  case class LinkedTableImpl[P, COL, OID: Cell, OTABLE <: AbstractTable[_], OLP, OP]
                            (columnFrom: P   ⇒ COL,
                             filteredOn: COL ⇒ FilteredTableRef[OID, OTABLE, OLP, OP, COL]) extends LinkedTable[P] {
    def view(foundRow: P) = {
      val ref = filteredOn(columnFrom(foundRow))
      crudAction.read(ref.query).zipMap(ref.extractIdFromRow) match {
        case Nil              ⇒ Renderer(ref) missingRow Some((ref.colName, ref.colValue))
        case (id, row) :: Nil ⇒ Renderer(ref) row (id, row)
        case idsRows          ⇒ Renderer(ref) rows idsRows
      }
    }
  }
}
