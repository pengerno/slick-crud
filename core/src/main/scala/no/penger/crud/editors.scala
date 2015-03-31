package no.penger
package crud

trait editors extends editorAbstracts with crudActions with renderers with syntax {

  case class Editor[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
                   (ref: TableRef[ID, TABLE, LP, P],
                    n:   UpdateNotifier) extends EditorAbstract[ID]{

    override val idCell    = ref.metadata.idCell
    override val mountedAt = ref.base.mounted
    override val tableName = ref.metadata.tableName

    override def create(req: REQ, params: Map[ColumnName, String]) =
      crudAction.create(ref.base, params) biMap (
        errors ⇒ CreateFailed(mountedAt, tableName, errors)               andThen n.notifyUpdateFailure(req),
        id     ⇒ Created(     mountedAt, tableName, id.map(idCell.toStr)) andThen n.notifyUpdated(req)
      )

    override def update(req: REQ, id: ID, columnName: ColumnName, value: String) =
      crudAction.update(ref, id, columnName, value) biMap (
        error            ⇒ UpdateFailed(mountedAt, tableName, columnName, idCell.toStr(id), value, error) andThen n.notifyUpdateFailure(req),
        {case (from, to) ⇒ Updated(     mountedAt, tableName, columnName, idCell.toStr(id), from, to)     andThen n.notifyUpdated(req)}
      )

    override def delete(req: REQ, id: ID) =
      crudAction.delete(ref.base, id) biMap (
        error ⇒ DeleteFailed(mountedAt, tableName, idCell.toStr(id), error) andThen n.notifyUpdateFailure(req),
        _     ⇒ Deleted(     mountedAt, tableName, idCell.toStr(id))        andThen n.notifyUpdated(req)
      )

    override def message(msg: String): PageFormat = Renderer(ref) message msg

    override val viewNew = Renderer(ref) createRow None
    
    override def view(page: Int) = {
      val (pos, read) = crudAction.read(ref.query, page, ref.base.pageSize)
      read.zipMap(ref.metadata.extractIdFromRow) match {
        case Nil if ref.base.isEditable ⇒ Renderer(ref) createRow None
        case Nil                        ⇒ Renderer(ref) noRow None
        case idsRows                    ⇒ Renderer(ref) rows (tableName, isLinked = false, pos, idsRows, None)
      }
    }

    override def viewRow(id: ID) = {
      val rowRef      = Some((ref.metadata.idColName, Some(id)))
      val (pos, read) = crudAction.read(ref.queryById(id), 0, ref.base.pageSize)
      read match {
        case Nil if ref.base.isEditable ⇒ Renderer(ref) createRow rowRef
        case Nil                        ⇒ Renderer(ref) noRow rowRef
        case row :: Nil                 ⇒ ref.linked.foldLeft(Renderer(ref) row(tableName, Some(id), ref.base.canDelete, row, rowRef))(
          (acc, linked) ⇒ combine(acc, linked.lookupAndApply(id, viewLinked))
        )
        case idsRows                    ⇒ Renderer(ref) rows (tableName, isLinked = false, pos, idsRows zipMap ref.metadata.extractIdFromRow, rowRef)
      }
    }

    object viewLinked extends LinkedTableF1[PageFormat]{
      override def apply[OID, OTABLE <: AbstractTable[_], OLP, OP, OC, C]
                        (ref: FilteredTableRef[OID, OTABLE, OLP, OP, OC, C]) = {
        val (pos, read) = crudAction.read(ref.query, 0, ref.base.pageSize)

        read.zipMap(ref.metadata.extractIdFromRow) match {
          case Nil if ref.base.isEditable      ⇒ Renderer(ref.wrapped) createRow Some((ref.filterColumn, None))
          case Nil                             ⇒ Renderer(ref.wrapped) noRow Some((ref.filterColumn, None))
          case (oid, (referenced, row)) :: Nil ⇒ Renderer(ref.wrapped) row (ref.metadata.tableName, oid, ref.base.canDelete, row, Some((ref.filterColumn, referenced)))
          case rows                        ⇒
            val (idRows, referees) = rows.foldLeft[(Seq[(Option[OID], OP)], Set[C])]((Seq.empty, Set.empty)){
              case ((idRows_, referees_), (id, (referee, row))) ⇒ (idRows_ :+ ((id, row)), referees_ + referee)
            }

            Renderer(ref.wrapped) rows (ref.metadata.tableName, isLinked = true, pos, idRows, Some((ref.filterColumn, referees)))
        }
      }
    }
  }
}