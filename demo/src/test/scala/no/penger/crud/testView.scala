package no.penger.crud

trait testView extends view {

  override type ElemFormat = String
  override type PageFormat = Seq[TestView]

  type Row = Seq[String]

  override def append(one: PageFormat, two: PageFormat) = one ++ two

  case class TestView(tableName: TableName,
                      columnNames: Seq[ColumnName],
                      content: Either[(Option[String], Option[Row]), Seq[Row]]){
    def id = content.left.map(_._1)

    def rows = content match {
      case Left((id, row)) => row.toSeq
      case Right(rows)     => rows
    }
  }

  override def View(ctx: String, uniqueId: String, tableName: TableName, columnNames: Seq[ColumnName]) = new View {
    override def many(rows: Seq[Row]): PageFormat =
      Seq(TestView(tableName, columnNames, Right(rows)))

    override def rowOpt(id: Option[String], rowOpt: Option[Row]) =
      Seq(TestView(tableName, columnNames, Left(id, rowOpt)))

    override def newPage: PageFormat = Seq.empty
  }
}