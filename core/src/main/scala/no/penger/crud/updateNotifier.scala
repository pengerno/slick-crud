package no.penger.crud

trait updateNotifier {
  trait UpdateNotifier{
    def updated[ID, T](t: TableName, id: ID)(u: Update)
    def updateFailed[ID](t: TableName, id: ID)(f: FailedUpdate)
  }
}