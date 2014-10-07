package no.penger.crud

trait view extends viewFormat {

  def append(one: PageFormat, two: PageFormat): PageFormat

  def View(ctx: String, uniqueId: String, tableName: TableName, columnNames: Seq[TableColumn]): View

  trait View{
    def many(rows: Seq[Seq[ElemFormat]]): PageFormat
    def rowOpt(id: Option[ElemFormat], rowOpt: Option[Seq[ElemFormat]]): PageFormat
    def newPage: PageFormat
  }
}