package no.penger.crud

trait updateNotifier {
  class UpdateNotifier {
    def updated[ID, T](t: TableName, id: ID)(u: Update) = ()
    def updateFailed[ID](t: TableName, id: ID)(f: FailedUpdate) = ()
    def addedRow[ID](t: TableName, id: Option[ID]) = ()
  }
}