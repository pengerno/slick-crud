package no.penger
package crud

trait editors extends editorAbstracts with crudActions with renderers with updateNotifier with syntax {

  case class Editor[ID, TABLE <: AbstractTable[_], LP, P]
                   (ref: TableRef[ID, TABLE, LP, P],
                    n:   UpdateNotifier) extends EditorAbstract[ID]{

    override val idCell    = ref.metadata.idCell
    override val mountedAt = ref.base.mounted
    override val tableName = ref.metadata.tableName

    override def create(params: Map[ColumnName, String]) =
      crudAction.create(ref.base, params) biMap (
        errors ⇒ CreateFailed(tableName, errors) andThen n.notifyUpdateFailure,
        id     ⇒ Created(     tableName, id)     andThen n.notifyUpdated
      )

    override def update(id: ID, columnName: ColumnName, value: String) =
      crudAction.update(ref, id, columnName, value) biMap (
        error       ⇒ UpdateFailed(tableName, id, columnName, value, error)         andThen n.notifyUpdateFailure,
        oldNew      ⇒ Updated(     tableName, id, columnName, oldNew._2, oldNew._1) andThen n.notifyUpdated
      )

    override def delete(id: ID) =
      crudAction.delete(ref.base, id) biMap (
        error ⇒ DeleteFailed(tableName, id, error) andThen n.notifyUpdateFailure,
        _     ⇒ Deleted(     tableName, id)        andThen n.notifyUpdated
      )

    override def message(msg: String): PageFormat = Renderer(ref) message msg

    override val viewNew = Renderer(ref) createRow None
    
    override def view = crudAction.read(ref.query).zipMap(ref.metadata.extractIdFromRow) match {
      case Nil if ref.base.isEditable ⇒ Renderer(ref) createRow None
      case Nil                        ⇒ Renderer(ref) noRow None
      case idsRows                    ⇒ Renderer(ref) rows (tableName, idsRows, None)
    }

    override def viewRow(id: ID) = {
      val rowRef = Some((ref.metadata.idColName, Some(id)))
      crudAction.read(ref.queryById(id)) match {
        case Nil if ref.base.isEditable ⇒ Renderer(ref) createRow rowRef
        case Nil                        ⇒ Renderer(ref) noRow rowRef
        case row :: Nil                 ⇒ ref.linked.foldLeft(Renderer(ref) row(tableName, Some(id), row, rowRef))(
          (acc, linked) ⇒ combine(acc, linked.lookupAndApply(id, viewLinked))
        )
        case idsRows                    ⇒ Renderer(ref) rows (tableName, idsRows zipMap ref.metadata.extractIdFromRow, rowRef)
      }
    }

    object viewLinked extends LinkedTableF1[PageFormat]{
      override def apply[OID, OTABLE <: AbstractTable[_], OLP, OP, OC, C]
                        (ref: FilteredTableRef[OID, OTABLE, OLP, OP, OC, C]) = {
        crudAction.read(ref.query).zipMap(ref.metadata.extractIdFromRow) match {
          case Nil if ref.base.isEditable      ⇒ Renderer(ref.wrapped) createRow Some((ref.filterColumn, None))
          case Nil                             ⇒ Renderer(ref.wrapped) noRow Some((ref.filterColumn, None))
          case (oid, (referenced, row)) :: Nil ⇒ Renderer(ref.wrapped) row (ref.metadata.tableName, oid, row, Some((ref.filterColumn, referenced)))
          case rows                        ⇒
            val (idRows, referees) = rows.foldLeft[(Seq[(Option[OID], OP)], Set[C])]((Seq.empty, Set.empty)){
              case ((idRows_, referees_), (id, (referee, row))) ⇒ (idRows_ :+ ((id, row)), referees_ + referee)
            }

            Renderer(ref.wrapped) rows (ref.metadata.tableName, idRows, Some((ref.filterColumn, referees)))
        }
      }
    }
  }
}