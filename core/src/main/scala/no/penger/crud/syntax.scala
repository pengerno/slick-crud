package no.penger.crud

import slick.lifted.CanBeQueryCondition

trait syntax extends tableLinks with cellInstances {
  import profile.simple._

  object TableRef {
    /**
     * A reference to a slick table
     *
     * @param table the TableQuery[E] of the table we want to expose
     * @param idCol a function that maps the default projection 'TABLE'
     *              to its primary key column.
     *              Multi-column primary keys are not supported.
     */
    def apply[ID: ColumnType: Cell, TABLE <: AbstractTable[_]]
      (mounted:     String,
       table:       TableQuery[TABLE],
       isEditable:  Boolean = true,
       canDelete:   Boolean = false,
       pageSize:    Option[Int] = None)
      (idCol:       TABLE ⇒ Rep[ID])
      (implicit cr: CellRow[TABLE#TableElementType]) = {
      BaseTableRef[ID, TABLE](mounted, table, isEditable, canDelete, pageSize, idCol)
    }
  }

  implicit class TableRefOps[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]){

    def projected[QLP, QP](q: Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq])
                          (implicit cr: CellRow[QP]): TableRef[ID, TABLE, QLP, QP] =
      ProjectedTableRef[ID, TABLE, LP, P, QLP, QP](ref, q)

    /**
     * Combine this editor with another database table referenced by 'other' on
     *  where 'columnFrom' of a given database row matches 'other's' 'columnQuery'
     */
    def linkedOn[OID, OTABLE <: AbstractTable[_], OLP, OP, C: Cell: FlatRepShape, OC: FlatRepShape, R]
      (fromCol: LP ⇒ Rep[C],
       other:   ⇒ TableRef[OID, OTABLE, OLP, OP])
      (toCol: OLP ⇒ Rep[OC])
      (pred: (Rep[C], Rep[OC]) ⇒ Rep[R])
      (implicit ev: CanBeQueryCondition[Rep[R]]): TableRef[ID, TABLE, LP, P] =
      ReferencingTableRef[ID, TABLE, LP, P, C, OID, OTABLE, OLP, OP, OC, R](ref, fromCol, toCol, pred)(other)

    def linked: List[LinkedTable[ID]] = {
      def linkedInner(r: TableRef[ID, _, _, _]): List[LinkedTable[ID]] = r match {
        case   BaseTableRef(_, _, _, _, _, _)           ⇒ Nil
        case   FilteredTableRef(wrapped, _, _)          ⇒ linkedInner(wrapped)
        case l@ReferencingTableRef(wrapped, _, _, _)    ⇒ linkedInner(wrapped) :+ l.link
        case   ProjectedTableRef(wrapped, _)            ⇒
          val res: List[LinkedTable[ID]] = linkedInner(wrapped)
          if (res.isEmpty) Nil else sys.error("Use linkedOn() after projected()")
      }
      linkedInner(ref)
    }
  }

}
