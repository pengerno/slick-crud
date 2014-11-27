
package no.penger
package crud

trait editors extends editorAbstracts with crudActions with renderers with updateNotifier {
  import profile.simple._

  case class Editor[ID: BaseColumnType : Cell, TABLE <: AbstractTable[_], LP, P]
                   (ref: TableRef[ID, TABLE, LP, P],
                    n:   UpdateNotifier) extends EditorAbstract[ID]{

    override val idCell    = implicitly[Cell[ID]]
    override val mountedAt = ref.base.mounted
    override val tableName = ref.base.tableName

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

    override val viewNew = Renderer(ref) missingRow None

    override def view = crudAction.read(ref.query).zipMap(ref.extractIdFromRow) match {
      case Nil     ⇒ Renderer(ref) missingRow None
      case idsRows ⇒ Renderer(ref) rows idsRows
    }

    override def viewRow(id: ID) = crudAction.read(ref.queryById(id)) match {
      case Nil        ⇒ Renderer(ref) missingRow Some((ref.base.primaryKey, id))
      case row :: Nil ⇒ ref.linked.foldLeft(Renderer(ref) row(id, row))((acc, linked) ⇒ combine(acc, linked(row, viewLinked)))
      case idsRows    ⇒ Renderer(ref) rows (idsRows zipMap ref.extractIdFromRow)
    }

    object viewLinked extends LinkedTableF1[P, PageFormat]{
      override def apply[OID: Cell, OTABLE <: AbstractTable[_], OLP, OP, COL](ref: FilteredTableRef[OID, OTABLE, OLP, OP, COL]) = {
        crudAction.read(ref.query).zipMap(ref.extractIdFromRow) match {
          case Nil              ⇒ Renderer(ref) missingRow Some((ref.filterColumn, ref.colValue))
          case (id, row) :: Nil ⇒ Renderer(ref) row (id, row)
          case idsRows          ⇒ Renderer(ref) rows idsRows
        }
      }
    }
  }
}