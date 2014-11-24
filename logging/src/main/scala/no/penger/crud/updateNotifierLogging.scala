package no.penger.crud

import com.typesafe.scalalogging.LazyLogging

trait updateNotifierLogging extends updateNotifier {
  trait UpdateNotifierLogging extends UpdateNotifier {
    self: LazyLogging ⇒

    override def notifyUpdated(s: CrudSuccess) = s match {
      case Created(t, id)                 ⇒ logger.info(s"Added row for table $t with id $id")
      case Updated(t, id, col, new_, old) ⇒ logger.info(s"Updated table $t for row $id for column $col from $old to $new_")
      case Deleted(t, id)                 ⇒ logger.info(s"Deleted row for table $t with id $id")
    }

    def warn(e: Error, msg: String) = e match {
      case ErrorExc(t)      ⇒ logger.warn(msg, t)
      case ErrorMsg(detail) ⇒ logger.warn(s"$msg: $detail")
    }

    def errorMessages(es: Seq[Error]) = es.map {
      case ErrorExc(t)   ⇒ t.getMessage
      case ErrorMsg(msg) ⇒ msg
    }.mkString(", ")

    override def notifyUpdateFailure(f: CrudFailure) = f match {
      case CreateFailed(t, errors)            ⇒ errors.toList match {
        case firstError :: Nil ⇒ warn(firstError, s"Failed to create new row for table $t")
        case _                 ⇒ logger.warn(s"Failed to create new row for table $t: ${errorMessages(errors)}")
      }
      case UpdateFailed(t, id, col, value, error) ⇒ warn(error, s"Failed to update row $id in table $t for column $col and value $value")
      case DeleteFailed(t, id, error)             ⇒ warn(error, s"Failed to delete row $id in table $t because $error")
    }
  }
}
