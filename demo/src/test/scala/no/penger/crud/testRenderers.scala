package no.penger.crud

trait testRenderers extends renderers {

  override type ElemFormat = String
  override type PageFormat = Seq[TestView]

  type Row = Seq[String]

  override def combine(one: PageFormat, two: PageFormat) = one ++ two

  case class TestView(
    tableName:  TableName,
    cells:      Seq[(ColumnName, Cell[Any])],
    content:    Either[(Option[String], Option[Row]), Seq[Row]]){

    def id = content.left.map(_._1)

    def rows = content match {
      case Left((id, row)) ⇒ row.toSeq
      case Right(rows)     ⇒ rows
    }
  }
  override def Renderer[ID, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) =
    new Renderer[ID, P] {
      def renderRow(row: P): Seq[ElemFormat] =
        ref.metadata.cellsWithUnpackedValues(row).map {
          case ((name, c), value) ⇒ c.toStr(value)
        }.toIndexedSeq

      override def rows[T](rows: Seq[(Option[ID], P)], via: Option[(ColumnName, T)]): PageFormat =
        Seq(TestView(ref.base.tableName, ref.metadata.cells, Right(rows.map(r ⇒ renderRow(r._2)))))

      override def row[T](idOpt: Option[ID], row: P, via: Option[(ColumnName, T)]): PageFormat =
        Seq(TestView(ref.base.tableName, ref.metadata.cells, Left((Some(idOpt.fold("missing")(ref.metadata.idCell.toStr)), Some(renderRow(row))))))

      override def createRow[T](knownColumn: Option[(ColumnName, Option[T])]): PageFormat = Seq.empty

      override def noRow[T](knownColumn: Option[(ColumnName, Option[T])]) = Seq.empty
    }
}