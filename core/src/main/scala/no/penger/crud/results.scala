package no.penger.crud

trait results extends errors {
  sealed trait CrudSuccess
  sealed trait CrudFailure

  case class Created[ID](table: TableName, id: Option[ID]) extends CrudSuccess
  case class Updated[ID](table: TableName, id: ID, column: ColumnName, newValue: String, oldValue: String) extends CrudSuccess
  case class Deleted[ID](table: TableName, id: ID) extends CrudSuccess

  case class CreateFailed(table: TableName, ts: Seq[Error]) extends CrudFailure
  case class UpdateFailed[ID](table: TableName, id: ID, column: ColumnName, value: String, e: Error) extends CrudFailure
  case class DeleteFailed[ID](table: TableName, id: ID, e: Error) extends CrudFailure

}
