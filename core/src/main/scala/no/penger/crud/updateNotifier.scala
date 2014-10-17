package no.penger.crud

trait updateNotifier {
  class UpdateNotifier {
    def updated[ID, T](t: TableName, id: ID)(u: UpdateSuccess) = ()
    def create[ID](t: TableName, id: ID) = ()
    def deleted[ID](t: TableName, id: ID) = ()

    def updateFailed[ID](t: TableName, id: ID)(f: UpdateFailed) = ()
    def deleteFailed[ID](t: TableName, id: ID)(d: DeleteFailed) = ()
  }
}