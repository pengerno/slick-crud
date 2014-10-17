package no.penger.crud

trait editorAbstracts extends viewFormat with cells {
  trait EditorAbstract[ID] {
    val mounted: String
    def idCell: Cell[ID]
    def tableName: TableName
    def view: PageFormat
    def viewRow(id: ID): PageFormat
    def viewNew: PageFormat
    def create(params: Map[ColumnName, String]): Either[Seq[Throwable], ID]
    def update(id: ID, updates: Map[ColumnName, String]): Either[Seq[UpdateFailed], Seq[UpdateSuccess]]
    def delete(id:ID): Either[DeleteFailed, DeleteSuccess.type]
  }
}
