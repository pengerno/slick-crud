package no.penger.crud

trait results extends errors {
  sealed trait CrudSuccess
  sealed trait CrudFailure

  case class Created(table: TableName, id: Option[String]) extends CrudSuccess
  case class Updated(table: TableName, column: ColumnName, row: String, oldValue: Option[String], newValue: String) extends CrudSuccess
  case class Deleted(table: TableName, id: String) extends CrudSuccess

  case class CreateFailed(table: TableName, ts: Seq[Error]) extends CrudFailure
  case class UpdateFailed(table: TableName, column: ColumnName, id: String, value: String, e: Error) extends CrudFailure
  case class DeleteFailed(table: TableName, id: String, e: Error) extends CrudFailure

}
