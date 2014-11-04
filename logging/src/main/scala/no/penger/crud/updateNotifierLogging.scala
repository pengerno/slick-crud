package no.penger.crud

import com.typesafe.scalalogging.LazyLogging

trait updateNotifierLogging extends updateNotifier {
  trait UpdateNotifierLogging extends UpdateNotifier {
    self: LazyLogging ⇒

    override def notifyUpdated(s: res.Success) = s match {
      case res.Created(t, id)                 ⇒ logger.info(s"Added row for table $t with id $id")
      case res.Updated(t, id, col, old, new_) ⇒ logger.info(s"Updated table $t for row $id for column $col from $old to $new_")
      case res.Deleted(t, id)                 ⇒ logger.info(s"Deleted row for table $t with id $id")
    }

    def warn(e: Error, msg: String) = e match {
      case ErrorExc(t)      ⇒ logger.warn(msg, t)
      case ErrorMsg(detail) ⇒ logger.warn(s"$msg: $detail")
    }

    def errorMessages(es: Seq[Error]) = es.map {
      case ErrorExc(t)   ⇒ t.getMessage
      case ErrorMsg(msg) ⇒ msg
    }.mkString(", ")

    override def notifyUpdateFailure(f: res.Failure) = f match {
      case res.CreateFailed(t, errors)            ⇒ errors.toList match {
        case firstError :: Nil ⇒ warn(firstError, s"Failed to create new row for table $t")
        case _                 ⇒ logger.warn(s"Failed to create new row for table $t: ${errorMessages(errors)}")
      }
      case res.UpdateFailed(t, id, error, col, value) ⇒ warn(error, s"Failed to update row $id in table $t for column $col and value $value")
      case res.DeleteFailed(t, id, error)             ⇒ warn(error, s"Failed to delete row $id in table $t because $error")
    }
  }
}
