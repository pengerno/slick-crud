package no.penger.crud

trait tableRefs extends namedCellRows with slickIntegration {
  import profile.simple._
  
  /**
   * A foreign key read mapping parametrized on the base tables' row projection type
   */
  sealed abstract class LinkedTable[P] {
    /* lookup referred row and apply it to 'f' */
    def apply[T](foundRow: P, f: LinkedTableF1[P, T]): T
  }

  /**
   * This is essentially a partially applied foreign key mapping query
   *  where we package up and hide types and query details. `foundRow` is injected
   *  only when we display the row in question
   */
  trait LinkedTableF1[P, T]{
    def apply[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL](ref: FilteredTableRef[OID, OTABLE, OLP, OP, COL]): T
  }

  object TableRef{
    /**
     * A reference to a slick table
     *
     * @param table the TableQuery[E] of the table we want to expose
     * @param idCol a function that maps the default projection 'TABLE'
     *              to its primary key column.
     *              Multi-column primary keys are not supported.
     */
    def apply[ID: BaseColumnType: Cell, TABLE <: AbstractTable[_]]
             (mounted:     String,
              table:       TableQuery[TABLE],
              isEditable:  Boolean = true)
             (idCol:       TABLE ⇒ Column[ID])
             (implicit cr: CellRow[TABLE#TableElementType]) =
      BaseTableRef[ID, TABLE](mounted, table, isEditable, NamedCellRow(table), idCol)
  }

  /**
   *  A reference to a slick table

   *  The names of type parameters of this table abstraction are reused within
   *   the whole codebase. Sometimes you will find Q(L)P or O(L)P, in which case
   *   they refer to a query or reference to another table, respectively.
   *
   * @tparam ID the primary key column, for example Column[Int]
   * @tparam TABLE row type for a database reference, ie. the class of the table definition
   * @tparam LP the lifted projection, for example (Column[Int], Column[String])
   * @tparam P the projection, for example (Int, String)
   */
  sealed abstract class TableRef[ID: Cell, TABLE <: AbstractTable[_], LP, P]{
    def base:              BaseTableRef[ID, TABLE]
    def cells:             NamedCellRow[P]
    def query:             Query[LP, P, Seq]
    def queryById(id: ID): Query[LP, P, Seq]
    def linked:            List[LinkedTable[P]]

    def projected[QLP, QP](q: Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq])(implicit cr: CellRow[QP]): TableRef[ID, TABLE, QLP, QP] =
      ProjectedTableRef[ID, TABLE, LP, P, QLP, QP](this, q, NamedCellRow(q(query)))

    def filtered[COL: BaseColumnType](on: LP ⇒ Column[COL])(value: COL) =
      FilteredTableRef[ID, TABLE, LP, P, COL](this, on, value)

    /**
     * Combine this editor with another database table referenced by 'other' on
     *  where 'columnValue' of a given database row matches 'other's' 'columnQuery'
     *
     * Linked tables are exposed when /one/ row of the base table is exposed.
     */
    def linkedOn[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL: BaseColumnType : Cell]
                (columnFrom:  P ⇒ COL,
                 other:       TableRef[OID, OTABLE, OLP, OP])
                (columnQuery: OLP ⇒ Column[COL]): TableRef[ID, TABLE, LP, P] =

      ReferencingTableRef[ID, TABLE, LP, P, OID, OTABLE, OLP, OP, COL](this, columnFrom, other, columnQuery)

    private[crud] def extractIdFromRow(row: P) =
      cells.extractCell(row, base.primaryKey, base.idCell)
  }

  case class BaseTableRef[ID: BaseColumnType: Cell, P <: AbstractTable[_]]
                         (mounted:    String,
                          query:      TableQuery[P],
                          isEditable: Boolean,
                          cells:      NamedCellRow[P#TableElementType],
                          idCol:      P ⇒ Column[ID]) extends TableRef[ID, P, P, P#TableElementType]{

    val tableName:  TableName  = AstParser.tableName(query)
    val primaryKey: ColumnName = AstParser.colNames(query.map(idCol)).head
    val idCell:     Cell[ID]   = implicitly[Cell[ID]]

    override val linked            = Nil
    override val base              = this
    override def queryById(id: ID) = query.filter(row ⇒ idCol(row) === id)
  }

  case class ProjectedTableRef[ID: Cell, TABLE <: AbstractTable[_], LP, P, QLP, QP]
                              (wrapped: TableRef[ID, TABLE, LP, P],
                               proj:    Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq],
                               cells:   NamedCellRow[QP]) extends TableRef[ID, TABLE, QLP, QP]{

    override val base               = wrapped.base
    override val linked             = if (wrapped.linked.isEmpty) Nil else sys.error("Use linkedOn() after projected()")
    override val query              = proj(wrapped.query)
    override def queryById(id: ID)  = proj(wrapped.queryById(id))
  }

  case class FilteredTableRef[ID: Cell, TABLE <: AbstractTable[_], LP, P, COL: BaseColumnType]
                             (wrapped:   TableRef[ID, TABLE, LP, P],
                              columnFor: LP ⇒ Column[COL],
                              colValue:  COL) extends TableRef[ID, TABLE, LP, P]{

    def filter(q: Query[LP, P, Seq]) = q.filter(columnFor(_) === colValue)

    val filterColumn                = AstParser.colNames(wrapped.query.map(columnFor)).head
    override val base               = wrapped.base
    override val cells              = wrapped.cells
    override val linked             = wrapped.linked
    override val query              = filter(wrapped.query)
    override def queryById(id: ID)  = filter(wrapped.queryById(id))
  }

  case class ReferencingTableRef[ID: Cell, TABLE <: AbstractTable[_], LP, P, OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL: BaseColumnType]
                                (wrapped:     TableRef[ID, TABLE, LP, P],
                                 columnFrom:  P ⇒ COL,
                                 other:       TableRef[OID, OTABLE, OLP, OP],
                                 columnQuery: OLP ⇒ Column[COL]) extends TableRef[ID, TABLE, LP, P]{

    val link: LinkedTable[P] = new LinkedTable[P] {
      override def apply[T](foundRow: P, f: LinkedTableF1[P, T]) =
        f(other.filtered(on = columnQuery)(value = columnFrom(foundRow)))
    }

    override val base               = wrapped.base
    override val cells              = wrapped.cells
    override val linked             = link +: wrapped.linked
    override val query              = wrapped.query
    override def queryById(id: ID)  = wrapped.queryById(id)
  }
}
