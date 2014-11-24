package no.penger.crud

trait updateNotifier {

  class UpdateNotifier {
    def notifyUpdated(s: CrudSuccess) = ()
    def notifyUpdateFailure(s: CrudFailure) = ()
  }
}