package no.penger.crud

import com.typesafe.scalalogging.slf4j.LazyLogging

trait updateNotifierLogging extends updateNotifier {
  trait UpdateNotifierLogging extends UpdateNotifier {
    self: LazyLogging ⇒

    override def updated[ID, T](t: TableName, id: ID)(u: Update) =
      logger.info(s"updated table $t for row $id for column ${u.column} from ${u.oldValue} to ${u.newValue}")

     override def updateFailed[ID](t: TableName, id: ID)(f: FailedUpdate) =
      logger.warn(s"could not update table $t for row $id for column ${f.column} and value ${f.value}}", f.t)

     override def addedRow[ID](t: TableName, id: ID) =
      logger.warn(s"added row for table $t with id $id")
  }
}
