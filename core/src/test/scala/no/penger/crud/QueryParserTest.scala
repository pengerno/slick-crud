package no.penger.crud

import no.penger.db
import org.scalatest.FunSuite

class QueryParserTest
  extends FunSuite
  with QueryParserModule
  with db.H2TransactionComponent {

  def db = ???
  import profile.simple._

  def myAssert[E, U, R](q: Query[E, U, Seq], shouldEqual: R)(op: Query[E, U, Seq] => R) = {
    assertResult(shouldEqual, q.selectStatement)(op(q))
  }
  def tc(t: String, c: String) = TableColumn(TableName(t), ColumnName(c))

  test("understand simple columns"){
    class OneTwoThreeT(tag: Tag) extends Table[(Int, Option[Int], Option[Int])](tag, "t") {
      def one   = column[Int]("one")
      def two   = column[Option[Int]]("two")
      def three = column[Int]("three").?

      def * = (one, two, three)
    }
    myAssert(TableQuery[OneTwoThreeT], Seq(tc("t", "one"), tc("t", "two"), tc("t", "three")))(QueryParser.columns)
  }

  test("understand map"){
    class OneTwoThreeT(tag: Tag) extends Table[(String, Option[String], Option[String])](tag, "t") {
      def one   = column[String]("one")
      def two   = column[Option[String]]("two")
      def three = column[String]("three").?

      def * = (one, two, three)
    }
    myAssert(TableQuery[OneTwoThreeT].map(_.two), Seq(tc("t", "two")))(QueryParser.columns)
    myAssert(TableQuery[OneTwoThreeT].map(t => (t.two, t.three)), Seq(tc("t", "two"), tc("t", "three")))(QueryParser.columns)
  }

  test("understand case class projection"){
    case class Strings(s1: String, os1: Option[String], os2: Option[String])
    class OneTwoThreeT(tag: Tag) extends Table[Strings](tag, "t") {
      def one   = column[String]("one")
      def two   = column[Option[String]]("two")
      def three = column[String]("three").?

      def * = (one, two, three) <> (Strings.tupled, Strings.unapply)
    }
    myAssert(TableQuery[OneTwoThreeT], Seq(tc("t", "one"), tc("t", "two"), tc("t", "three")))(QueryParser.columns)
  }

  test("understand query"){
    case class Strings(s1: String, os1: Option[String], os2: Option[String])
    class OneTwoThreeT(tag: Tag) extends Table[Strings](tag, "t") {
      def one   = column[String]("one")
      def two   = column[Option[String]]("two")
      def three = column[String]("three").?

      def * = (one, two, three) <> (Strings.tupled, Strings.unapply)
    }
    myAssert(TableQuery[OneTwoThreeT].sortBy(_.two.asc), Seq(tc("t", "one"), tc("t", "two"), tc("t", "three")))(QueryParser.columns)
  }

  test("understand joins"){
    case class Strings(s1: String, os1: Option[String], os2: Option[String])
    class OneT(tag: Tag) extends Table[(String, String)](tag, "t1") {
      def one = column[String]("one")
      def two = column[String]("two")
      def * = (one, two)
    }
    class TwoT(tag: Tag) extends Table[(String, String)](tag, "t2") {
      def one = column[String]("one")
      def two = column[String]("two")
      def * = (one, two)
    }
    class ThreeT(tag: Tag) extends Table[(String, String, Option[String])](tag, "t3") {
      def one = column[String]("one")
      def two = column[String]("two")
      def three = column[String]("three").?
      def * = (one, two, three)
    }

    myAssert(TableQuery[OneT].join(TableQuery[TwoT]),
      Seq(tc("t1", "one"), tc("t1", "two"), tc("t2", "one"), tc("t2", "two")))(QueryParser.columns)

    myAssert(TableQuery[OneT].join(TableQuery[TwoT].map(_.two)),
      Seq(tc("t1", "one"), tc("t1", "two"), tc("t2", "two")))(QueryParser.columns)

    myAssert(TableQuery[OneT].join(TableQuery[TwoT].rightJoin(TableQuery[ThreeT].map(_.three))),
      Seq(tc("t1", "one"), tc("t1", "two"), tc("t2", "one"), tc("t2", "two"), tc("t3", "three")))(QueryParser.columns)
  }

  test("get tablename"){
    class OneT(tag: Tag) extends Table[String](tag, "t1") {
      def one = column[String]("one")
      def * = one
    }

    val one = QueryParser.tablenameFrom(TableQuery[OneT])
    assertResult(TableName("t1"))(one)
  }
}

