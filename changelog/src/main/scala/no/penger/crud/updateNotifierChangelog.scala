package no.penger.crud

import java.sql.Timestamp

import org.joda.time.DateTime

trait updateNotifierChangelog extends updateNotifier with dbIntegration {
  trait UpdateNotifierChangelog extends UpdateNotifier {
    val changelogTableName = "crud_changelog"

    import profile.simple._
    implicit val m0 = MappedColumnType.base[TableName,  String](_.toString, TableName)
    implicit val m1 = MappedColumnType.base[ColumnName, String](_.toString, ColumnName)
    implicit val m2 = MappedColumnType.base[DateTime, Timestamp](dt ⇒ new Timestamp(dt.getMillis), ts ⇒ DateTime.now().withMillis(ts.getTime))

    class ChangelogT(t: Tag) extends Table[(Long, TableName, ColumnName, String, Option[String], String, DateTime, String)](t, changelogTableName){
      def id          = column[Long]      ("id", O.PrimaryKey, O.AutoInc)
      def table       = column[TableName] ("table")
      def col         = column[ColumnName]("column")
      def row         = column[String]    ("row")
      def from        = column[String]    ("from").?
      def to          = column[String]    ("to")
      def timestamp   = column[DateTime]  ("timestamp")
      def userDetails = column[String]    ("user_details")

      def * = (id, table, col, row, from, to, timestamp, userDetails)
    }
    val Changelog = TableQuery[ChangelogT]

    override abstract def notifyUpdated(req: REQ)(s: CrudSuccess) = {
      super.notifyUpdated(req)(s)
      s match {
        case Updated(t, col, row, from, to) ⇒
          db.withSession(implicit s ⇒ Changelog.insert((0L, t, col, row, from, to, DateTime.now, userDetails(req))))
          ()
        case _ ⇒ ()
      }
    }
  }
}
