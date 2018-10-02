package no.penger.crud

import java.sql.Timestamp

import org.joda.time.DateTime

trait updateNotifierChangelog extends updateNotifier with dbIntegration {
  trait UpdateNotifierChangelog extends UpdateNotifier {
    val changelogTableName = "crud_changelog"

    import profile.api._
    implicit val m0 = MappedColumnType.base[TableName,  String](_.toString, TableName)
    implicit val m1 = MappedColumnType.base[ColumnName, String](_.toString, ColumnName)
    implicit val m2 = MappedColumnType.base[DateTime, Timestamp](dt ⇒ new Timestamp(dt.getMillis), ts ⇒ DateTime.now().withMillis(ts.getTime))

    class ChangelogT(t: Tag) extends Table[(Long, String, TableName, ColumnName, String, Option[String], String, DateTime, String)](t, changelogTableName){
      def id           = column[Long]      ("id", O.PrimaryKey, O.AutoInc)
      def table        = column[TableName] ("table_name")
      def tableMounted = column[String]    ("table_mounted")
      def col          = column[ColumnName]("column_name")
      def row          = column[String]    ("row_id")
      def from         = column[String]    ("from_value").?
      def to           = column[String]    ("to_value")
      def timestamp    = column[DateTime]  ("changed_at")
      def userDetails  = column[String]    ("user_details")

      def * = (id, tableMounted, table, col, row, from, to, timestamp, userDetails)
    }
    val Changelog = TableQuery[ChangelogT]

    override abstract def notifyUpdated(req: REQ)(s: CrudSuccess) = {
      super.notifyUpdated(req)(s)
      s match {
        case Updated(mountedAt, t, col, row, from, to) ⇒
          db.run(Changelog += ((0L, mountedAt, t, col, row, from, to, DateTime.now, userDetails(req)))).await
          ()
        case _ ⇒ ()
      }
    }
  }
}
