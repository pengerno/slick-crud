package no.penger.crud

trait tableRefs extends namedCellRows with slickIntegration {
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
             (implicit cr: CellRow[TABLE#TableElementType]) = {
      BaseTableRef[ID, TABLE](mounted, table, isEditable, NamedCellRow(table).withPkCell(table.map(idCol)), /* todo: remove:*/ idCol)
    }
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
    def linked:            List[LinkedTable[ID]]

    def projected[QLP, QP](q: Query[LP, P, Seq] ⇒ Query[QLP, QP, Seq])(implicit cr: CellRow[QP]): TableRef[ID, TABLE, QLP, QP] =
      ProjectedTableRef[ID, TABLE, LP, P, QLP, QP](this, q, NamedCellRow(q(query)).withOverriddenCells(base.cells))

    def filtered[COL: BaseColumnType](on: LP ⇒ Column[COL])(value: COL) =
      FilteredTableRef[ID, TABLE, LP, P, COL](this, on, value)

    /**
     * Combine this editor with another database table referenced by 'other' on
     *  where 'columnFrom' of a given database row matches 'other's' 'columnQuery'
     */
    def linkedOn[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL: BaseColumnType : Cell]
                (columnFrom:  LP ⇒ Column[COL],
                 other:       TableRef[OID, OTABLE, OLP, OP])
                (columnQuery: OLP ⇒ Column[COL]): TableRef[ID, TABLE, LP, P] =

      ReferencingTableRef[ID, TABLE, LP, P, OID, OTABLE, OLP, OP, COL](this, columnFrom, other, columnQuery)

    private[crud] def extractIdFromRow(row: P) = cells.extractCell(row, base.primaryKey, base.idCell)
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

  case class ReferencingTableRef[ ID: Cell,  TABLE <: AbstractTable[_],  LP,  P,
                                 OID: Cell, OTABLE <: AbstractTable[_], OLP, OP,
                                 COL: BaseColumnType: Cell]
                                (from:     TableRef[ID, TABLE, LP, P],
                                 fromCol:  LP ⇒ Column[COL],
                                 to:       TableRef[OID, OTABLE, OLP, OP],
                                 toCol:    OLP ⇒ Column[COL]) extends TableRef[ID, TABLE, LP, P]{

    val link = new LinkedTable[ID]{
      /* f(select OP from 'to' where fromCol(from) is toCol(to)) */
      override def lookupAndApply[T](id: ID, f: LinkedTableF1[T]) = {
        val value       = db.withSession(implicit s ⇒ from.queryById(id).map(fromCol).first)
        val filteredRef = to.filtered(toCol)(value)
        f(filteredRef)
      }
    }
    val cellLink = FKCell(to.query.map(toCol).selectStatement, db.withSession(implicit s ⇒ to.query.map(toCol).list)) _

    override val base               = from.base
    override val cells              = from.cells.withFkCell(from.query.map(fromCol), cellLink)
    override val linked             = from.linked :+ link
    override val query              = from.query
    override def queryById(id: ID)  = from.queryById(id)
  }
}
