package no.penger.crud

import scala.language.implicitConversions
import scala.slick.ast.ScalaBaseType
import scala.util.{Failure, Success, Try}

trait crudActions extends tableRefs with columnPicker with dbIntegration {

  import profile.simple._

  object crudAction {
    import slickHacks._

    def read[E, U](q: Query[E, U, Seq]): List[U] =
      db withSession (implicit s ⇒ q.list)

    /**
     * Update a column 'columnName' for row with id 'id' with value 'value'
     *
     * We take two sets of named cells here. 'namedCellsQuery' just to verify that the
     *  columns to be updated are exposed by the current query, and we use 'namedCellsTable'
     *  for the actual update
     *
     *  @return old value of cell on success, error otherwise
     */
    def update[ID, TABLE <: AbstractTable[_], LP, P]
              (ref:        TableRef[ID, TABLE, LP, P],
               id:         ID,
               columnName: ColumnName,
               value:      String): Either[Error, (String, String)] =
      for {
        _              ← ref.metadata cellByName columnName orError s"projection has no cell with name $columnName"
        cell           ← ref.base.metadata cellByName columnName orError s"table has no cell with name $columnName"
        validValue     ← cell fromStr value
        row            = ref.base.queryById(id)
        updater        = row map (slickTable =>
                            (findColumnWithName(slickTable, columnName) map ensureOptionalColumn(validValue)).get
                          )
        oldValueOpt    ← Try(db withSession (implicit s ⇒ updater.firstOption)).toEither(errorExc)
        _              ← db withTransaction (implicit s ⇒ ensureOneRowChanged(Try(updater update validValue)))
      } yield (oldValueOpt.toString, validValue.toString)

    def create[ID, TABLE <: AbstractTable[_]]
              (ref:    BaseTableRef[ID, TABLE],
               params: Map[ColumnName, String]): Either[Seq[Error], Option[ID]] = {

      def doInsert(toInsert: TABLE#TableElementType): Either[Seq[Error], Option[ID]] =
      /* first try and insert and see if we can get an id back */
        Try[Option[ID]](Some(db withTransaction (implicit s ⇒ ref.query returning ref.query.map(ref.idCol) insert toInsert))).orElse {
          /* try again without return of autoincrement value */
          Try(db withTransaction (implicit s ⇒ ref.query.insert(toInsert))).map{
            /* since there was no auto-generated id, dig out the id from what we inserted */
            _ ⇒ ref.metadata.extractIdFromRow(toInsert)
          }
        }.toEither(t ⇒ Seq(errorExc(t)))

      for {
        toInsert ← ref.metadata.parseRow(params)
        id       ← doInsert(toInsert)
      } yield id
    }

    def delete[ID, TABLE <: AbstractTable[_]]
              (ref: BaseTableRef[ID, TABLE], id: ID): Either[Error, Unit] =
      db withTransaction (implicit s ⇒ ensureOneRowChanged(Try(ref.queryById(id).delete)))

    private def ensureOneRowChanged(tn: Try[Int])(implicit s: Session) = tn match {
      case Success(1)   ⇒ Right(())
      case Success(0)   ⇒ s.rollback(); Left(errorMsg(s"No rows matched"))
      case Success(num) ⇒ s.rollback(); Left(errorMsg(s"Rolled back because matched $num rows"))
      case Failure(t)   ⇒ s.rollback(); Left(errorExc(t))
    }
  }

  object slickHacks{
    /* the default implicit, queryToDeleteInvoker, only converts Query[_ <: Table[_]]) to DeleteInvoker,
     *  so I changed it here to make it work for Query[_ <: AbstractTable[_]] too */
    implicit def queryToDeleteInvoker(q: profile.simple.Query[_ <: AbstractTable[_], _, Seq]): profile.DeleteInvoker =
      profile.createDeleteInvoker(profile.deleteCompiler.run(q.toNode).tree, ())

    /* to be able to use Column[Any].? */
    private implicit val evidence: ScalaBaseType[Any] = null

    /* this is needed to hack around a case where a column is declared as column[T], but used in
     *   the table projection as a column[Option[T]]
     */
    def ensureOptionalColumn(value: Any)(c: Column[Any]) =
      (value, c.toNode) match {
        case (v: Option[_], slick.ast.OptionApply(_)) => c
        case (v: Option[_], _                       ) => c.?.asInstanceOf[Column[Any]]
        case _                                        => c
      }

  }
}