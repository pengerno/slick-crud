package no.penger.crud

import org.scalatest.FunSuite

class AstParserTest
  extends FunSuite
  with astParser
  with slick.driver.JdbcDriver {

  import simple._

  def myAssert[E, U, R](q: Query[E, U, Seq], shouldEqual: R)(op: Query[E, U, Seq] ⇒ R) = {
    assertResult(shouldEqual, q.selectStatement)(op(q))
  }

  def c(c: String) = ColumnName(c)

  class OneTwoThreeT(tag: Tag) extends Table[(Int, Option[Int], Option[Int])](tag, "t") {
    def one   = column[Int]("one")
    def two   = column[Option[Int]]("two")
    def three = column[Int]("three").?

    def * = (one, two, three)
  }

  case class Strings(s1: String, os1: Option[String], os2: Option[String])
  class OneTwoThreeST(tag: Tag) extends Table[Strings](tag, "t") {
    def one   = column[String]("one")
    def two   = column[Option[String]]("two")
    def three = column[String]("three").?

    def * = (one, two, three) <> (Strings.tupled, Strings.unapply)
  }

  test("understand simple columns"){
    myAssert(TableQuery[OneTwoThreeT], Seq(c("one"), c("two"), c("three")))(AstParser.colNames)
  }

  test("understand map to one column "){
    myAssert(TableQuery[OneTwoThreeT].map(_.two), Seq(c("two")))(AstParser.colNames)
  }

  test("understand map to two columns "){
    myAssert(TableQuery[OneTwoThreeT].map(t ⇒ (t.two, t.three)), Seq(c("two"), c("three")))(AstParser.colNames)
  }

  test("understand map / nested projections "){
    myAssert(TableQuery[OneTwoThreeT].sortBy(_.two).map(t ⇒ (t.two, t.three)).map(_._1), Seq(c("two")))(AstParser.colNames)
  }

  test("understand case class projection"){
    myAssert(TableQuery[OneTwoThreeST], Seq(c("one"), c("two"), c("three")))(AstParser.colNames)
  }

  test("understand query"){
    myAssert(TableQuery[OneTwoThreeST].sortBy(_.two.asc), Seq(c("one"), c("two"), c("three")))(AstParser.colNames)
  }

  test("understand query with join"){
    class TT(tag: Tag) extends Table[String](tag, "t2") {
      def one   = column[String]("one")
      def * = one
    }
    myAssert(TableQuery[OneTwoThreeST].join(TableQuery[TT]).on(_.one === _.one).map(_._1),
             Seq(c("one"), c("two"), c("three")))(AstParser.colNames)
  }

  test("get tablename"){
    val one = AstParser.tableName(TableQuery[OneTwoThreeT])
    assertResult(TableName("t"))(one)
  }
}