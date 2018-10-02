package no.penger.crud

import slick.ast.BaseTypedType

import scala.language.implicitConversions
import scala.util.{Failure, Success}

trait crudActions extends tableRefs with columnPicker with dbIntegration with positions {

  import profile.api._


  object crudAction {
    import slickHacks._

    def read[E, U](q: Query[E, U, Seq], page: Int, pageSizeOpt: Option[Int]): (Position, List[U]) =
      pageSizeOpt match {
        case Some(pageSize) => {
          val transaction = for {
            total <- q.size.result
            pos = PagedPosition(page * pageSize, Math.min((page + 1) * pageSize, total), total, page)
            res <- q.drop(page * pageSize).take(pageSize).result
          } yield (pos, res.toList)

          db.run(transaction.transactionally).await
        }
        case _ =>
          (NotPaged, db.run(q.result).await.toList)
    }


    /**
     * Update a column 'columnName' for row with id 'id' with value 'value'
     *
     * We take two sets of named cells here. 'namedCellsQuery' just to verify that the
     *  columns to be updated are exposed by the current query, and we use 'namedCellsTable'
     *  for the actual update
     *
     *  @return old value of cell on success, error otherwise
     */
    def update[ID: FlatRepShape, TABLE <: AbstractTable[_], LP, P]
              (ref:        TableRef[ID, TABLE, LP, P],
               id:         ID,
               columnName: ColumnName,
               value:      String): Either[Error, (Option[String], String)] = {

        for {
          _              ← ref.metadata cellByName columnName orError s"projection has no cell with name $columnName"
          cell           ← ref.base.metadata cellByName columnName orError s"table has no cell with name $columnName"
          validValue     ← cell fromStr value
          row            = ref.base.queryById(id)
          updater        = row map (slickTable =>
                              (findColumnWithName(slickTable, columnName) map ensureOptionalColumn(validValue)).get
                            )
          oldValueOpt    ← db.run(updater.result.headOption.asTry).await.asEither(errorExc)
          _              ←
            db.run(
              updater
                .update(validValue)
                .flatMap(i => ensureOneRowChangedDBIO(i))
                .transactionally
                .asTry
                .flatMap{
                  case Success(_) => DBIO.successful(Right(()))
                  case Failure(f) => DBIO.successful(Left(errorExc(f)))
                }
            ).await
        } yield (oldValueOpt map cell.toStr, value)
      }

    def create[ID: FlatRepShape, TABLE <: AbstractTable[_]]
    (ref:    BaseTableRef[ID, TABLE],
     params: Map[ColumnName, String]): Either[Seq[Error], Option[ID]] = {
      def doInsert(toInsert: TABLE#TableElementType): Either[Seq[Error], Option[ID]] = {
        def autoId = (ref.query.returning(ref.query.map(ref.idCol)) += toInsert).transactionally.map(f => Option[ID](f)).asTry
        def getId = (ref.query += toInsert).map(_ => ref.metadata.extractIdFromRow(toInsert)).transactionally.asTry

        db.run(autoId).await.orElse(db.run(getId).await).asEither(t ⇒ Seq(errorExc(t)))
      }

      for {
        toInsert ← ref.metadata.parseRow(params)
        id       ← doInsert(toInsert)
      } yield id
    }

    def delete[ID, TABLE <: AbstractTable[_]](ref: BaseTableRef[ID, TABLE], id: ID): Either[Error, Unit] =
      if(ref.base.canDelete) {
        val transaction =
          ref
            .queryById(id)
            .delete
            .flatMap(i => ensureOneRowChangedDBIO(i))
            .transactionally
            .asTry
            .flatMap{
              case Success(_) => DBIO.successful(Right(()))
              case Failure(f) => DBIO.successful(Left(errorExc(f)))
            }

        db.run(transaction).await
      } else {
        Left(errorMsg("Can not delete"))
      }

    private def ensureOneRowChangedDBIO(tn: Int): DBIOAction[Int, NoStream, Effect] = tn match {
      case 1   ⇒ DBIO.successful(1)
      case 0   ⇒ DBIO.failed(new Throwable(s"No rows matched"))
      case num ⇒ DBIO.failed(new Throwable(s"Rolled back because matched $num rows"))
    }
  }

  object slickHacks{
    /* the default implicit, queryToDeleteInvoker, only converts Query[_ <: Table[_]]) to DeleteInvoker,
     *  so I changed it here to make it work for Query[_ <: AbstractTable[_]] too */
    implicit def queryToDeleteInvoker(q: profile.api.Query[_ <: AbstractTable[_], _, Seq]): profile.DeleteActionExtensionMethods =
      profile.createDeleteActionExtensionMethods(profile.deleteCompiler.run(q.toNode).tree, ())

    /* enable Rep[Any] - we have to guarantee on the outside that what we do is sane */
    implicit val anyEvidence: BaseTypedType[Any] = null

    /* this is needed to hack around a case where a column is declared as column[T], but used in
     *   the table projection as a column[Option[T]]
     */
    def ensureOptionalColumn(value: Any)(c: Rep[Any]) =
      (value, c.toNode) match {
        case (v: Option[_], slick.ast.OptionApply(_)) => c
        case (v: Option[_], _                       ) => c.?.asInstanceOf[Rep[Any]]
        case _                                        => c
      }
  }
}