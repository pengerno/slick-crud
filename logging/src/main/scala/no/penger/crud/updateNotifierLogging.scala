package no.penger.crud

import com.typesafe.scalalogging.LazyLogging

trait updateNotifierLogging extends updateNotifier {
  trait UpdateNotifierLogging extends UpdateNotifier {
    self: LazyLogging ⇒

    override abstract def notifyUpdated(req: REQ)(s: CrudSuccess) = {
      super.notifyUpdated(req)(s)
      s match {
        case Created(t, id)                 ⇒ logger.info(s"Added row for table $t with id $id")
        case Updated(t, col, id, from, to)  ⇒ logger.info(s"Updated table $t for column $col for id $id from ${from.fold("empty")(f ⇒ s"«$f»")} to «$to»")
        case Deleted(t, id)                 ⇒ logger.info(s"Deleted row for table $t with id $id")
      }
    }

    override abstract def notifyUpdateFailure(req: REQ)(f: CrudFailure) = {
      super.notifyUpdateFailure(req)(f)

      def warn(e: Error, msg: String) = e match {
        case ErrorExc(t)      ⇒ logger.warn(msg, t)
        case ErrorMsg(detail) ⇒ logger.warn(s"$msg: $detail")
      }

      def errorMessages(es: Seq[Error]) = es.map {
        case ErrorExc(t)   ⇒ t.getClass.getName + ": " + t.getMessage
        case ErrorMsg(msg) ⇒ msg
      }.mkString(", ")

      f match {
        case CreateFailed(t, errors)                ⇒ errors.toList match {
          case firstError :: Nil ⇒ warn(firstError, s"Failed to create new row for table $t")
          case _                 ⇒ logger.warn(s"Failed to create new row for table $t: ${errorMessages(errors)}")
        }
        case UpdateFailed(t, id, col, value, error) ⇒ warn(error, s"Failed to update row $id in table $t for column $col and value $value")
        case DeleteFailed(t, id, error)             ⇒ warn(error, s"Failed to delete row $id in table $t because $error")
      }
    }
  }
}
