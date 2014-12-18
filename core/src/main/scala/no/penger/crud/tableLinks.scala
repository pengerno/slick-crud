package no.penger.crud

trait tableLinks extends tableRefs {
  import profile.simple._

  sealed trait LinkedTable[ID] {
    def lookupAndApply[T](id: ID, f: LinkedTableF1[T]): T
  }

  /**
   * This is essentially a partially applied foreign key mapping query
   *  where we package up and hide types and query details. `ref` is injected
   *  only when we display the row in question
   */
  trait LinkedTableF1[T]{
    def apply[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL](ref: FilteredTableRef[OID, OTABLE, OLP, OP, COL]): T
  }

  case class ReferencingTableRef[ ID: Cell,  TABLE <: AbstractTable[_],  LP,  P,
                                 OID: Cell, OTABLE <: AbstractTable[_], OLP, OP,
                                 COL: BaseColumnType: Cell]
                                 (from:     TableRef[ID, TABLE, LP, P],
                                  fromCol:  LP ⇒ Column[COL],
                                  to:       TableRef[OID, OTABLE, OLP, OP],
                                  toCol:    OLP ⇒ Column[COL]) extends TableRef[ID, TABLE, LP, P]{

    val link: LinkedTable[ID] = new LinkedTable[ID]{
      /* f(select OP from 'to' where fromCol(from) is toCol(to)) */
      override def lookupAndApply[T](id: ID, f: LinkedTableF1[T]) = {
        val value       = db.withSession(implicit s ⇒ from.queryById(id).map(fromCol).first)
        val filteredRef = to.filtered(toCol)(value)
        f(filteredRef)
      }
    }
    private val fkCellWrapper: (Cell[COL]) ⇒ FKCell[COL] = FKCell(
      to.query.map(toCol).selectStatement,
      db.withSession(implicit s ⇒ to.query.map(toCol).list)
    )

    override val base               = from.base
    override val metadata           = from.metadata.withFkCell(from.query.map(fromCol), fkCellWrapper)
    override val query              = from.query
    override def queryById(id: ID)  = from.queryById(id)
  }
}