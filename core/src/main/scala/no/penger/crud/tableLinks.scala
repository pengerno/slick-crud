package no.penger.crud

import slick.lifted.CanBeQueryCondition

trait tableLinks extends tableRefs with dbIntegration {
  import profile.api._

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

  case class ReferencingTableRef[ ID,  TABLE <: AbstractTable[_],  LP,  P,  C: FlatRepShape: Cell,
                                 OID, OTABLE <: AbstractTable[_], OLP, OP, OC: FlatRepShape,
                                  R](
                                     from:    TableRef[ID, TABLE, LP, P],
                                     fromCol: LP               ⇒ Rep[C],
                                     toCol:   OLP              ⇒ Rep[OC],
                                     pred:   (Rep[C], Rep[OC]) ⇒ Rep[R])
                                    (_to:                      ⇒ TableRef[OID, OTABLE, OLP, OP])
                           (implicit ev: CanBeQueryCondition[Rep[R]]) extends TableRef[ID, TABLE, LP, P] {
    lazy val to = _to

    lazy val link: LinkedTable[ID] = new LinkedTable[ID]{
      /* f(select OP from 'to' where fromCol(from) is toCol(to)) */
      override def lookupAndApply[T](id: ID, f: LinkedTableF1[T]) = {
        val filteredQ: Query[OLP, OP, Seq] ⇒ Query[(Rep[C], OLP), (C, OP), Seq] =
          toQuery ⇒ from.queryById(id).map(fromCol).join(toQuery).on((f, to) ⇒ pred(f, toCol(to)))

        f(FilteredTableRef[OID, OTABLE, OLP, OP, OC, C](to, filteredQ, toCol))
      }
    }
    private lazy val fkCellWrapper: (Cell[OC]) ⇒ ConstrainedCell[OC] = {
      val q = to.query.map(toCol)
      val transaction = for {
        s <- q.size.result
        a <- if(s < maxNumLinks) q.result.map(Option(_)) else DBIO.successful(Option.empty)
      } yield a

      cell ⇒ ConstrainedCell(cell, to.query.map(toCol).result.statements.headOption)(db.run(transaction.transactionally).await)
    }

    override val base               = from.base
    override val metadata           = from.metadata.withFkCell(from.query.map(fromCol), fkCellWrapper)
    override val query              = from.query
    override def queryById(id: ID)  = from.queryById(id)
  }
}
