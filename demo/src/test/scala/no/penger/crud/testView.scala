package no.penger.crud

trait testView extends view {

  override type ElemFormat = String
  override type PageFormat = Seq[TestView]

  type Row = Seq[String]

  override def append(one: PageFormat, two: PageFormat) = one ++ two

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

  override def View[ID: Cell, ROW](base: String, tableName: TableName, isEditable: Boolean, id: ColumnName, namedCells: NamedCells[ROW]): View[ID, ROW] =
    new View[ID, ROW] {
      def renderRow(row: ROW): Seq[ElemFormat] =
        namedCells.cellsWithUnpackedValues(row).map {
          case ((name, cell), value) ⇒ renderCell(name, value, cell)
        }.toIndexedSeq

      override def renderCell(columnName: ColumnName, value: Any, cell: Cell[Any]) =
        cell.toStr(value)

      override def notFound(idOpt: Option[ID]) =
        Seq(TestView(tableName, namedCells.cells, Left((idOpt.map(id ⇒ Cell.toStr(id)), None))))

      override def many(rows: Seq[(ID, ROW)]): PageFormat =
        Seq(TestView(tableName, namedCells.cells, Right(rows.map(r ⇒ renderRow(r._2)))))

      override def single(id: ID, row: ROW): PageFormat =
        Seq(TestView(tableName, namedCells.cells, Left((Some(Cell.toStr(id)), Some(renderRow(row))))))

      override def newPage: PageFormat = Seq.empty
  }
}