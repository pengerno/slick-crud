package no.penger.crud

trait editorAbstracts extends viewFormat with cells {
  trait EditorAbstract[ID] {
    def idCell: Cell[ID]
    def tableName: TableName
    def view(baseUrl: String): ViewFormat
    def viewRow(baseUrl: String, id: ID): ViewFormat
    def update(id: ID, updates: Map[ColumnName, String]): Either[Seq[FailedUpdate], Seq[Update]]
  }
}
