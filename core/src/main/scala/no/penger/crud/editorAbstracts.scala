package no.penger.crud

trait editorAbstracts extends viewFormat with cells {
  trait EditorAbstract[ID] {
    def idCell: Cell[ID]
    def tableName: TableName
    def view(baseUrl: String): PageFormat
    def viewRow(baseUrl: String, id: ID): PageFormat
    def update(id: ID, updates: Map[ColumnName, String]): Either[Seq[FailedUpdate], Seq[Update]]
  }
}
