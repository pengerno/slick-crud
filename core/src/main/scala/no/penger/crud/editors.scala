
package no.penger
package crud

trait editors extends editorAbstracts with linkedTables {
  import profile.simple._

  case class Editor[ID: BaseColumnType : Cell, TABLE <: AbstractTable[_], LP, P]
                   (ref:   TableRef[ID, TABLE, LP, P],
                    n:     UpdateNotifier,
                    links: Seq[LinkedTable[P]] = Seq.empty) extends EditorAbstract[ID]{

    val r = Renderer(ref)

    override val idCell    = implicitly[Cell[ID]]
    override val mountedAt = ref.base.mounted
    override val tableName = ref.base.tableName

    /**
     * Combine this editor with another database table referenced by 'other' on 
     *  where 'columnValue' of a given database row matches 'other's' 'columnQuery'
     * 
     * Linked tables are exposed when /one/ row of the base table is exposed.
     * 
     * The split between 'Editor' and 'TableRef' enables exposed tables to
     *  have reciprocal links.
     */
    def linkedOn[OID, OTABLE <: AbstractTable[_], OLP, OP, COL: BaseColumnType : Cell]
                (columnValue: P ⇒ COL,
                 other:       TableRef[OID, OTABLE, OLP, OP])
                (columnQuery: OLP ⇒ Column[COL]) =
      copy(links = links :+ LinkedTableImpl(columnValue, other.filtered(columnQuery))(other.base.idCell))

    override def create(params: Map[ColumnName, String]) =
      crudAction.create(ref.base, params) biMap (
        errors ⇒ CreateFailed(tableName, errors) andThen n.notifyUpdateFailure,
        id     ⇒ Created(     tableName, id)     andThen n.notifyUpdated
      )

    override def update(id: ID, columnName: ColumnName, value: String) =
      crudAction.update(ref.cells, ref.base, id, columnName, value) biMap (
        error       ⇒ UpdateFailed(tableName, id, columnName, value, error)         andThen n.notifyUpdateFailure,
        oldNew      ⇒ Updated(     tableName, id, columnName, oldNew._2, oldNew._1) andThen n.notifyUpdated
      )

    override def delete(id: ID) =
      crudAction.delete(ref.base, id) biMap (
        error ⇒ DeleteFailed(tableName, id, error) andThen n.notifyUpdateFailure,
        _     ⇒ Deleted(     tableName, id)        andThen n.notifyUpdated
      )

    override def viewNew = r missingRow None

    override def view = crudAction.read(ref.query).zipMap(ref.extractIdFromRow) match {
      case Nil     ⇒ r missingRow None
      case idsRows ⇒ r rows idsRows
    }

    override def viewRow(id: ID) = crudAction.read(ref.queryById(id)) match {
      case Nil        ⇒ r missingRow Some((ref.base.primaryKey, id))
      case row :: Nil ⇒ links.foldLeft(r row(id, row))((acc, link) ⇒ combine(acc, link.view(row)))
      case idsRows    ⇒ r rows (idsRows zipMap ref.extractIdFromRow)
    }
  }
}