package no.penger.crud

trait editorAbstracts extends renderFormat with cells with results {
  /* this trait mostly exists to shed a few type parameters and hide implementation details */
  abstract class EditorAbstract[ID]{
    val mountedAt: String
    val idCell:    Cell[ID]
    val tableName: TableName

    def message(s: String): PageFormat
    def view: PageFormat
    def viewNew: PageFormat
    def viewRow(id: ID): PageFormat

    def create(params: Map[ColumnName, String]): Either[CreateFailed, Created]
    def update(id: ID, columnName: ColumnName, value: String): Either[UpdateFailed, Updated]
    def delete(id: ID): Either[DeleteFailed, Deleted]
  }
}
