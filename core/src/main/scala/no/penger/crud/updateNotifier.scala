package no.penger.crud

trait updateNotifier extends results {

  class UpdateNotifier {
    def notifyUpdated(s: CrudSuccess) = ()
    def notifyUpdateFailure(s: CrudFailure) = ()
  }
}