package no.penger.crud

trait testRenderers extends renderers {

  override type ElemFormat = String
  override type PageFormat = Seq[TestView]

  type Row = Seq[String]

  override def combine(one: PageFormat, two: PageFormat) = one ++ two

  case class TestView(
    tableName:  TableName,
    cells:      Seq[NamedUntypedCell],
    content:    Either[(Option[String], Option[Row]), Seq[Row]]){

    def id = content.left.map(_._1)

    def rows = content match {
      case Left((id, row)) ⇒ row.toSeq
      case Right(rows)     ⇒ rows
    }
  }
  override def Renderer[ID: Cell, TABLE <: AbstractTable[_], LP, P](ref: TableRef[ID, TABLE, LP, P]) =
    new Renderer[ID, P] {
      def renderRow(row: P): Seq[ElemFormat] =
        ref.cells.cellsWithUnpackedValues(row).map {
          case ((name, c), value) ⇒ cell(name, value, c)
        }.toIndexedSeq

      override def cell(columnName: ColumnName, value: Any, cell: Cell[Any]) =
        cell.toStr(value)

      override def rows(rows: Seq[(ID, P)]): PageFormat =
        Seq(TestView(ref.base.tableName, ref.cells.cells, Right(rows.map(r ⇒ renderRow(r._2)))))

      override def row(id: ID, row: P): PageFormat =
        Seq(TestView(ref.base.tableName, ref.cells.cells, Left((Some(Cell.toStr(id)), Some(renderRow(row))))))

      override def missingRow[T](knownColumn: Option[(ColumnName, T)]): PageFormat = Seq.empty
  }
}