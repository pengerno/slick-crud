package no.penger.crud

trait updateNotifier extends results {
  type REQ

  def userDetails(req: REQ): String

  class UpdateNotifier {
    def notifyUpdated(req: REQ)(s: CrudSuccess) = ()
    def notifyUpdateFailure(req: REQ)(s: CrudFailure) = ()
  }
}