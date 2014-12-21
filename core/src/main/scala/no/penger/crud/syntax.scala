package no.penger.crud

import scala.slick.lifted.CanBeQueryCondition

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
       isEditable:  Boolean = true)
      (idCol:       TABLE ⇒ Column[ID])
      (implicit cr: CellRow[TABLE#TableElementType]) = {
      BaseTableRef[ID, TABLE](mounted, table, isEditable, idCol)
    }
  }

  implicit class TableRefOps[ID, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]){

    def projected[QLP, QP](q: Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq])
                          (implicit cr: CellRow[QP]): TableRef[ID, TABLE, QLP, QP] =
      ProjectedTableRef[ID, TABLE, LP, P, QLP, QP](ref, q)

    /**
     * Combine this editor with another database table referenced by 'other' on
     *  where 'columnFrom' of a given database row matches 'other's' 'columnQuery'
     */
    def linkedOn[OID, OTABLE <: AbstractTable[_], OLP, OP, C: Cell, OC, R]
      (fromCol: LP ⇒ Column[C],
       other:   TableRef[OID, OTABLE, OLP, OP])
      (toCol: OLP ⇒ Column[OC])
      (pred: (Column[C], Column[OC]) ⇒ Column[R])
      (implicit ev: CanBeQueryCondition[Column[R]]): TableRef[ID, TABLE, LP, P] =
      ReferencingTableRef[ID, TABLE, LP, P, C, OID, OTABLE, OLP, OP, OC, R](ref, fromCol, other, toCol, pred)

    def linked: List[LinkedTable[ID]] = {
      def linkedInner(r: TableRef[ID, _, _, _]): List[LinkedTable[ID]] = r match {
        case   BaseTableRef(_, _, _, _)                 ⇒ Nil
        case   FilteredTableRef(wrapped, _, _)          ⇒ linkedInner(wrapped)
        case l@ReferencingTableRef(wrapped, _, _, _, _) ⇒ linkedInner(wrapped) :+ l.link
        case   ProjectedTableRef(wrapped, _)            ⇒
          val res: List[LinkedTable[ID]] = linkedInner(wrapped)
          if (res.isEmpty) Nil else sys.error("Use linkedOn() after projected()")
      }
      linkedInner(ref)
    }
  }

}
