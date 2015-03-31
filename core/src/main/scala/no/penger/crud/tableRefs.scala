package no.penger.crud

trait tableRefs extends tableMetadata with slickIntegration {
  import profile.simple._

  /**
   *  A reference to a slick table

   *  The names of type parameters of this table abstraction are reused within
   *   the whole codebase. Sometimes you will find Q(L)P or O(L)P, in which case
   *   they refer to a query or reference to another table, respectively.
   *
   * @tparam ID the primary key column, for example Rep[Int]
   * @tparam TABLE row type for a database reference, ie. the class of the table definition
   * @tparam LP the lifted projection, for example (Rep[Int], Rep[String])
   * @tparam P the projection, for example (Int, String)
   */
  abstract class TableRef[ID, TABLE <: AbstractTable[_], LP, P]{
    val base:              BaseTableRef[ID, TABLE]
    val metadata:          Metadata[ID, P]
    def query:             Query[LP, P, Seq]
    def queryById(id: ID): Query[LP, P, Seq]
  }

  case class BaseTableRef[ID: Cell: BaseColumnType, P <: AbstractTable[_]]
                         (mounted:    String,
                          query:      TableQuery[P],
                          isEditable: Boolean,
                          canDelete:  Boolean,
                          pageSize:   Option[Int],
                          idCol:      P ⇒ Rep[ID])
                         (implicit cr: CellRow[P#TableElementType]) extends TableRef[ID, P, P, P#TableElementType]{

    override val metadata          = Metadata.infer(TableName(query.baseTableRow.tableName), query, idCol)
    override val base              = this
    override def queryById(id: ID) = query.filter(row ⇒ idCol(row) === id)
  }

  case class ProjectedTableRef[ID, TABLE <: AbstractTable[_], LP, P, QLP, QP: CellRow]
                              (wrapped: TableRef[ID, TABLE, LP, P],
                               proj:    Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq]) extends TableRef[ID, TABLE, QLP, QP]{

    override val base               = wrapped.base
    override val query              = proj(wrapped.query)
    override def queryById(id: ID)  = proj(wrapped.queryById(id))
    override val metadata           = Metadata.derive(query, wrapped.metadata)
  }

  case class FilteredTableRef[ID, TABLE <: AbstractTable[_], LP, P, C: FlatRepShape, OC: Cell: FlatRepShape]
                             (wrapped:   TableRef[ID, TABLE, LP, P],
                              filterQ:   Query[LP, P, Seq] ⇒ Query[(Rep[OC], LP), (OC, P), Seq],
                              columnFor: LP ⇒ Rep[C]) extends TableRef[ID, TABLE, (Rep[OC], LP), (OC, P)]{

    val filterColumn                = AstParser.colNames(wrapped.query.map(columnFor)).head
    override val base               = wrapped.base
    override val query              = filterQ(wrapped.query)
    override val metadata           = Metadata.withReferencingRow(query, wrapped.metadata)
    override def queryById(id: ID)  = filterQ(wrapped.queryById(id))
  }
}
