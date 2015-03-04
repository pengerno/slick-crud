package no.penger.crud

trait testRenderers extends renderers {

  override type ElemFormat = String
  override type PageFormat = Seq[TestView]

  type Row = Seq[String]

  override def combine(one: PageFormat, two: PageFormat) = one ++ two

  case class TestView(
    tableName:  TableName,
    cells:      Seq[(ColumnInfo, Cell[Any])],
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

      override def message(s: String): Seq[TestView] =
        Seq(TestView(ref.metadata.tableName, ref.metadata.cells, Left((Some(s), None))))

      override def rows[T](mainTable: TableName, isLinked: Boolean, pos: Position, rows: Seq[(Option[ID], P)], via: Option[(ColumnInfo, T)]): Seq[TestView] =
        Seq(TestView(ref.metadata.tableName, ref.metadata.cells, Right(rows.map(r ⇒ renderRow(r._2)))))

      override def row[T](mainTable: TableName, idOpt: Option[ID], canDelete: Boolean, row: P, via: Option[(ColumnInfo, T)]): Seq[TestView] =
        Seq(TestView(ref.metadata.tableName, ref.metadata.cells, Left((Some(idOpt.fold("missing")(ref.metadata.idCell.toStr)), Some(renderRow(row))))))

      override def createRow[T](knownColumn: Option[(ColumnInfo, Option[T])]) = Seq.empty

      override def noRow[T](knownColumn: Option[(ColumnInfo, Option[T])]) = Seq.empty
    }
}