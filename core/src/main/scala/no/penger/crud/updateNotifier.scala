package no.penger.crud

trait updateNotifier {

  object res {
    sealed trait Success
    sealed trait Failure

    case class Created[ID](table: TableName, id: ID) extends Success
    case class Updated[ID](table: TableName, id: ID, column: ColumnName, oldValue: String, newValue: String) extends Success
    case class Deleted[ID](table: TableName, id: ID) extends Success

    case class CreateFailed(table: TableName, ts: Seq[Error]) extends Failure
    case class UpdateFailed[ID](table: TableName, id: ID, e: Error, column: ColumnName, value: String) extends Failure
    case class DeleteFailed[ID](table: TableName, id: ID, e: Error) extends Failure
  }

  class UpdateNotifier {
    def notifyUpdated(s: res.Success) = ()
    def notifyUpdateFailure(s: res.Failure) = ()
  }
}