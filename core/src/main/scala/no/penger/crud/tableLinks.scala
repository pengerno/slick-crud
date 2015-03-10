package no.penger.crud

import scala.slick.lifted.CanBeQueryCondition

trait tableLinks extends tableRefs with dbIntegration {
  import profile.simple._

  /* A suitably long number for a dropdown*/
  val maxNumLinks = 300

  sealed trait LinkedTable[ID] {
    def lookupAndApply[T](id: ID, f: LinkedTableF1[T]): T
  }

  /**
   * This is essentially a partially applied foreign key mapping query
   *  where we package up and hide types and query details. `ref` is injected
   *  only when we display the row in question
   */
  trait LinkedTableF1[T]{
    def apply[OID, OTABLE <: AbstractTable[_], OLP, OP, OC, C](ref: FilteredTableRef[OID, OTABLE, OLP, OP, OC, C]): T
  }

  case class ReferencingTableRef[ ID,  TABLE <: AbstractTable[_],  LP,  P, C: Cell,
                                 OID, OTABLE <: AbstractTable[_], OLP, OP, OC, R](
                                  from:    TableRef[ID, TABLE, LP, P],
                                  fromCol: LP ⇒ Column[C],
                                  toCol:   OLP ⇒ Column[OC],
                                  pred:   (Column[C], Column[OC]) ⇒ Column[R])
                                 (_to:      ⇒ TableRef[OID, OTABLE, OLP, OP])
                                 (implicit ev: CanBeQueryCondition[Column[R]]) extends TableRef[ID, TABLE, LP, P] {
    lazy val to = _to

    lazy val link: LinkedTable[ID] = new LinkedTable[ID]{
      /* f(select OP from 'to' where fromCol(from) is toCol(to)) */
      override def lookupAndApply[T](id: ID, f: LinkedTableF1[T]) = {
        val filteredQ: Query[OLP, OP, Seq] ⇒ Query[(Column[C], OLP), (C, OP), Seq] =
          toQuery ⇒ from.queryById(id).map(fromCol).join(toQuery).on((f, to) ⇒ pred(f, toCol(to)))

        f(FilteredTableRef[OID, OTABLE, OLP, OP, OC, C](to, filteredQ, toCol))
      }
    }
    private lazy val fkCellWrapper: (Cell[OC]) ⇒ ConstrainedCell[OC] =
      cell ⇒ ConstrainedCell(cell, Some(to.query.map(toCol).selectStatement))(db.withSession {
        implicit s ⇒
          val q = to.query.map(toCol)
            val num = q.size.run
            if (num < maxNumLinks) Some(q.list) else None
      })

    override val base               = from.base
    override val metadata           = from.metadata.withFkCell(from.query.map(fromCol), fkCellWrapper)
    override val query              = from.query
    override def queryById(id: ID)  = from.queryById(id)
  }
}
