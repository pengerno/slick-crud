package no.penger.crud

trait results extends errors {
  sealed trait CrudSuccess
  sealed trait CrudFailure

  case class Created(mountedAt: String, table: TableName, id: Option[String]) extends CrudSuccess
  case class Updated(mountedAt: String, table: TableName, column: ColumnName, row: String, oldValue: Option[String], newValue: String) extends CrudSuccess
  case class Deleted(mountedAt: String, table: TableName, id: String) extends CrudSuccess

  case class CreateFailed(mountedAt: String, table: TableName, ts: Seq[Error]) extends CrudFailure
  case class UpdateFailed(mountedAt: String, table: TableName, column: ColumnName, id: String, value: String, e: Error) extends CrudFailure
  case class DeleteFailed(mountedAt: String, table: TableName, id: String, e: Error) extends CrudFailure

}
