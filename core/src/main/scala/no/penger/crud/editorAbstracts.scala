package no.penger.crud

trait editorAbstracts extends viewFormat with cells with updateNotifier {
  trait EditorAbstract[ID] {
    val mounted: String
    def idCell: Cell[ID]
    def tableName: TableName
    def view: PageFormat
    def viewRow(id: ID): PageFormat
    def viewNew: PageFormat
    def create(params: Map[ColumnName, String]): Either[res.CreateFailed, res.Created[ID]]
    def update(id: ID, columnName: ColumnName, value: String): Either[res.UpdateFailed[ID], res.Updated[ID]]
    def delete(id: ID): Either[res.DeleteFailed[ID], res.Deleted[ID]]
  }
}
