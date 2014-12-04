package no.penger.crud

trait syntax extends tableLinks {
  import profile.simple._

  implicit class TableRefOps[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]){

    def projected[QLP, QP](q: Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq])
                          (implicit cr: CellRow[QP]): TableRef[ID, TABLE, QLP, QP] =
      ProjectedTableRef[ID, TABLE, LP, P, QLP, QP](ref, q)

    def filtered[COL: BaseColumnType](on: LP ⇒ Column[COL])(value: COL) =
      FilteredTableRef[ID, TABLE, LP, P, COL](ref, on, value)

    /**
     * Combine this editor with another database table referenced by 'other' on
     *  where 'columnFrom' of a given database row matches 'other's' 'columnQuery'
     */
    def linkedOn[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL: BaseColumnType : Cell]
      (columnFrom:  LP ⇒ Column[COL],
       other:       TableRef[OID, OTABLE, OLP, OP])
      (columnQuery: OLP ⇒ Column[COL]): TableRef[ID, TABLE, LP, P] =

      ReferencingTableRef[ID, TABLE, LP, P, OID, OTABLE, OLP, OP, COL](ref, columnFrom, other, columnQuery)

    def linked: List[LinkedTable[ID]] = {
      def linkedInner(r: TableRef[ID, _, _, _]): List[LinkedTable[ID]] = r match {
        case BaseTableRef(_, _, _, _)                ⇒ Nil
        case FilteredTableRef(wrapped, _, _)         ⇒ linkedInner(wrapped)
        case l@ReferencingTableRef(wrapped, _, _, _) ⇒ linkedInner(wrapped) :+ l.link
        case ProjectedTableRef(wrapped, _)           ⇒
          val res: List[LinkedTable[ID]] = linkedInner(wrapped)
          if (res.isEmpty) Nil else sys.error("Use linkedOn() after projected()")
      }
      linkedInner(ref)
    }
  }
}
